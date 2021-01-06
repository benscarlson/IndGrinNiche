#----
# Run locally:
#   hvset=4axes50pts
#   Rscript $scriptsP/hv/hvs_niche_set.r hvs/$hvset -p mc
# Run in on hpc:
#   cd ~/results/huj_eobs_test
#   sbatch ~/projects/rsf/config/hvs_niche_set_mpi.sh
# TODO:
#   should pass in hvjob and and optional path to hv folder. should default to looking for hypervolumes in wd/hvjob
#   break out step where I save volumes to csv as a seperate step

set.seed(5326)
spsm <- suppressPackageStartupMessages

options(rgl.useNULL = TRUE) #hypervolume loads rgl, this makes sure it doesn't open x11 session

spsm(library(docopt))
spsm(library(foreach))
spsm(library(glue))
spsm(library(hypervolume))
spsm(library(iterators))
spsm(library(tidyverse))

select <- dplyr::select
filter <- dplyr::filter

'
Calculates niche set (population) hypervolumes based on individual hypervolumes.
Uses niche_set.csv to determine the niche sets

Usage:
hvs_pop <hvP> [--hvPop=<hvPop>] [--distFact=<distFact>] [--parMethod=<parMethod>] [--cores=<cores>]

Options:
-h --help     Show this screen.
-v --version     Show version.
-s --hvPop=<hvPop> The path to store population hypervolumes. If not passed, defaults to <hvP>/hvs_niche_set.
-d --distFact=<distFact> The distance factor to use in hypervolume_set (union operation). Defaults to 1.
-p --parMethod=<parMethod>  Either <mpi | mc>. If not passed in, script will run sequentially.
-c --cores=<cores> Number of cores. Defaults to 4. Applicable if parMethod is mc, ignored if mpi

' -> doc

#---- parameters ----#

#Note docopt initializes parameters that are not passed in to NULL
ag <- docopt(doc, version = '0.1\n')

if(interactive()) {
  .pd <- '/Users/benc/projects/whitestork/results/stpp_models/huj_eobs'
  .hvP <- 'hvs/4axes50pts'
  .parMethod <- 'none'
  .hvPopP <- 'hvs/4axes50pts/hvs_niche_set_df3'
  .distFact <- 3
  .cores <- 4
} else {
  .pd <- getwd()
  .hvP <- ag$hvP
  .parMethod <- ifelse(is.null(ag$parMethod),'none',ag$parMethod) #need to set value b/c NULL will fail in if statement
  .hvPopP <- ifelse(is.null(ag$hvPop),NA,ag$hvPop)
  .distFact <- ifelse(is.null(ag$distFact),1,as.numeric(ag$distFact))
  .cores <- ifelse(is.null(ag$cores),4,as.numeric(ag$cores))
}

#print(ag); quit()

#---- paths ----#
hvP <- file.path(.pd,.hvP)
hvPopP <- ifelse(is.na(.hvPopP),file.path(hvP,'hvs_niche_set'),file.path(.pd,.hvPopP))
volPF <- file.path(hvPopP,'niche_set_vol.csv')

message(glue('.pd is {.pd}'))
message(glue('hvP is {hvP}'))
message(glue('hvPopP is {hvPopP}'))
message(glue('volPF is {volPF}'))

#---- functions ----#
diffmin <- function(t) round(difftime(Sys.time(), t, unit = "min"),2)

#---- load data ----#
nsets <- read_csv(file.path(.pd,'niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.pd,'niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

dir.create(hvPopP,recursive=TRUE,showWarnings = FALSE)
tibble(niche_set=character(),niche_set_vol=numeric()) %>% write_csv(volPF)

#----
#---- start cluster and register backend ----
#---- 
if(.parMethod=='mpi') {
  message('Registering backend doMPI')
  spsm(library(doMPI))

  #start the cluster. number of tasks, etc. are defined by slurm in the init script.
  message('Starting mpi cluster.')
  cl <- startMPIcluster(verbose=TRUE)
  registerDoMPI(cl)
  setRngDoMPI(cl) #set each worker to receive a different stream of random numbers
  
  `%mypar%` <- `%dopar%`
  
} else if(.parMethod=='mc') {
  #.cores <- strtoi(Sys.getenv('SLURM_CPUS_PER_TASK', unset=1)) #for testing on hpc
  message(glue('Registering backend doMC with {.cores} cores'))
  spsm(library(doMC))
  RNGkind("L'Ecuyer-CMRG")

  registerDoMC(.cores)
  
  `%mypar%` <- `%dopar%`
  
} else {
  message('No parallel method defined, running sequentially.')
  #foreach package as %do% so it is loaded even if the parallel packages are not
  `%mypar%` <- `%do%`
}

#----
#---- Niche set hypervolumes ----#
#----

setNames <- unique(niches$niche_set)

tsTot <- Sys.time()

hvFiles <- foreach(i=icount(length(setNames)),
                   .packages=c('hypervolume','dplyr','glue'),
                   .combine='rbind') %mypar% {
  #i <- 1
  tsEnt <- Sys.time()                   
  nicheSet <- niches %>% filter(niche_set==setNames[i])
  
  message(glue('Niche set {setNames[i]} has a total of {nrow(nicheSet)} niches.'))
  
  hvList<-lapply(file.path(hvP,'hvs',glue('{nicheSet$niche_name}.rds')), 
                 function(x) tryCatch(readRDS(x),error=function(e) NULL))
  hvList <- hvList[lengths(hvList) != 0] #https://stackoverflow.com/questions/33004238/r-removing-null-elements-from-a-list
  
  if(length(hvList)!=nrow(nicheSet)) {
    warning(glue('Have {length(nicheSet$niche_name)} niches but could only load {length(hvList)} hvs.'))
  }
  #convert the list to a HypervolumeList
  hvs <- do.call(hypervolume_join,hvList)
  
  # union of all hypervolumes by iteratively calculating hypervolume_set
  #TODO: could switch over to using unionHvs() in hv-sim project. 
  message(glue('Calculating population hypervolume for {setNames[i]}...'))

  hv1 <- hvs@HVList[[1]]
  
  for(j in 2:length(hvs@HVList)) {
    #j <- 2
    hv2 <- hvs@HVList[[j]]
    
    sink('/dev/null')
      hvSet <- hypervolume_set(hv1, hv2, distance.factor=.distFact, check.memory=FALSE, verbose=FALSE)
    sink()

    #the 4th slot contains the union of hv1 and hv2
    #set the union hypervolume to hv1, then union this with the next hypervolume on the next iteration
    hv1 <- hvSet@HVList$Union

  }

  #hv1 is now the union of all the hypervolumes
  hvPop <- hv1
  hvPop@Name <- setNames[i]
  saveRDS(hvPop,file.path(hvPopP,glue('{setNames[i]}.rds')))
  
  tibble(niche_set=hvPop@Name,pop_niche_vol=get_volume(hvPop)) %>%
    readr::write_csv(volPF,append=TRUE)

  message(glue('Completed {setNames[i]}, niche {i} of {length(setNames)} in {diffmin(tsEnt)} minutes.'))
  
}

message('Niche set estimation complete.')
message(glue('Total elapsed time: {diffmin(tsTot)} minutes.'))

message('Script complete.')

if(.parMethod=='mpi') { #seems nothing after mpi.quit() is executed, so make sure this is the last code
  #---- finalize doMPI ---
  closeCluster(cl)
  mpi.quit()
}