# Run locally: 
#   scriptsP=~/projects/rsf/src/scripts
#   cd ~/projects/whitestork/results/stpp_models/huj_eobs
#   Rscript $scriptsP/hv_pairwise.r 5axes2000pts1 -i 1 -u 3 -p mc -c 6
# TODO: could update so that the pairs list is passed in, which would be more general
# TODO: hve not tested this on the HPC
# TODO: update to include jaccard index calculation

library(docopt)

'
Compute hypervolume volumes for pairwise set operations (Intersection, union)

Usage:
hv_pairwise <hvjob> [--hvF=<hvF>] [--dfIntr=<dfIntr>] [--dfUnion=<dfUnion>] [--parMethod=<parMethod>] [--cores=<cores>]
hv_pairwise (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-f --hvF=<hvF> Folder containing hypervolumes. If not provided, defaults to hvs/<hvjob>
-i --dfIntr=<dfIntr> Distance factor for intersection operation
-u --dfUnion=<dfUnion> Distance factor for union operation
-p --parMethod=<parMethod>  Either <mpi | mc>. If not passed in, script will run sequentially.
-c --cores=<cores> Number of cores. Defaults to 4. Applicable if parMethod is mc, ignored if mpi
' -> doc

ag <- docopt(doc, version = '0.1\n') #Note docopt initializes parameters that are not passed in to NULL

set.seed(5326)
source('~/projects/rsf/src/startup.r')

options(rgl.useNULL = TRUE) #hypervolume loads rgl, this makes sure it doesn't open x11 session

spsm(library(foreach))
spsm(library(hypervolume))
spsm(library(iterators))

source('~/projects/rsf/src/funs/general.r')

#---- Parameters ----#
if(interactive()) {
  .pd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
  .hvjob <- '5axes2000pts1'
  .hvF <- NULL
  .dfIntr <- 1
  .dfUnion <- 3
  .parMethod <- 'none'
  .cores <- 4
} else {
  .pd <- getwd()
  .hvjob <- ag$hvjob
  .hvF <- ag$hvF
  .dfIntr <- as.numeric(ag$dfIntr)
  .dfUnion <- as.numeric(ag$dfUnion)
  .parMethod <- ifelse(is.null(ag$parMethod),'none',ag$parMethod) #need to set value b/c NULL will fail in if statement
  .cores <- ifelse(is.null(ag$cores),4,as.numeric(ag$cores))
}

#---- Paths ----#
hvP <- ifelse(is.null(.hvF),file.path(.pd,'hvs',.hvjob),.hvF)
pwPF <- file.path(hvP,'hv_pairwise.csv')

#---- set up files and directories ----#
tibble(hv_job=character(), 
       niche_name1=character(),
       niche_name2=character(),
       hv_intr_vol=numeric(),
       hv_union_vol=numeric()) %>% 
  write_csv(pwPF) #csv to store intersection 

#---- load data ----#
nsets <- read_csv(file.path(.pd,'niche_sets.csv'),col_types=cols()) %>% filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.pd,'niches.csv'),col_types=cols()) %>% filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

message(glue('Running script for {nrow(niches)} niches.'))

hvdf <- niches %>%
  select(niche_set,niche_name) %>%
  mutate(hv=map(niche_name,~{
    tryCatch(
      readRDS(file.path(hvP,'hvs',glue('{.}.rds'))),
      error=function(e) NULL) %>%
      return()
  }))

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
#---- run set operations ----
#---- 

tsTot <- Sys.time()

for(nset in unique(niches$niche_set)) {
  #nset <- unique(niches$niche_set)[1]
  message(glue("Calculating pairwise set operations for {nset}"))
  
  pairs <- hvdf %>% 
    filter(niche_set==nset) %>%
    pluck('niche_name') %>%
    combn(2,simplify=FALSE)

  message(glue("Total of unique {length(pairs)} pairs"))
  
  foreach(i=icount(length(pairs)),
                     .packages=c('hypervolume','dplyr','glue','readr'),
                     .combine='rbind') %mypar% { #%do% 
    #i<-1                  
    pair <- sort(pairs[[i]]) #pairs should always be alphabetical.
    message(glue("Pair: {paste(pair,collapse=',')}"))
    
    hv1 <- hvdf %>% filter(niche_name==pair[1]) %>% pluck('hv') %>% pluck(1)
    hv2 <- hvdf %>% filter(niche_name==pair[2]) %>% pluck('hv') %>% pluck(1)

    stopifnot(hv1@Name==pair[1]) #little sanity test
    stopifnot(hv2@Name==pair[2])

    sink("/dev/null") #suppress all output, function is very noisy 
    intr <- hypervolume_set(hv1,hv2,verbose=FALSE,check.memory=FALSE, distance.factor=.dfIntr) %>%
      pluck('HVList') %>% pluck('Intersection')
    union <- hypervolume_set(hv1,hv2,verbose=FALSE,check.memory=FALSE, distance.factor=.dfUnion) %>%
      pluck('HVList') %>% pluck('Union')
    sink()
    
    tibble(hv_job=.hvjob,
           niche_name1=hv1@Name,
           niche_name2=hv2@Name,
           hv_intr_vol=as.numeric(get_volume(intr)),
           hv_union_vol=as.numeric(get_volume(union))) %>%
      write_csv(pwPF,append=TRUE)

    message(glue("Completed pair {i} of {length(pairs)}"))
    return(TRUE)
  }
}

message(glue('Total elapsed time: {diffmin(tsTot)} minutes.'))
message('Script complete.')
