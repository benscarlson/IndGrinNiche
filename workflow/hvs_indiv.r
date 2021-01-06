#----
# Run locally:
#   scriptsP=~/projects/rsf/src/scripts
#   axes=pct_tree,pct_bare,ndvi,dist2forest
#
#   cd ~/projects/whitestork/results/stpp_models/huj_eobs_test
#   Rscript $scriptsP/hv/hvs_indiv.r data/obsbg_anno.csv hvs/4axes50pts2 50 $axes -p mc
# Run in on hpc:
#   cd ~/results/huj_eobs_test
#   sbatch ~/projects/rsf/config/hvs_indiv_mpi.sh
#
# TODO: use like this to create empy tibble: tibble(niche_name=character(),event=character(),time=character())

spsm <- suppressPackageStartupMessages

options(rgl.useNULL = TRUE) #hypervolume loads rgl, this makes sure it doesn't open x11 session

spsm(library(docopt))
spsm(library(hypervolume))
spsm(library(foreach))
spsm(library(glue))
spsm(library(dplyr))
spsm(library(readr))

select <- dplyr::select
filter <- dplyr::filter

'
Parallel generation of hypervolumes

Usage:
hvs_indiv <dat> <hvP> <npts> <axes> [--parMethod=<parMethod>]
hvs_indiv (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-p --parMethod=<parMethod>  Either <mpi | mc>. If not passed in, script will run sequentially.

' -> doc

#Note docopt initializes parameters that are not passed in to NULL
ag <- docopt(doc, version = '0.1\n')

if(interactive()) {
  .pd <- '~/projects/whitestork/results/stpp_models/huj_eobs_test'
  .annoPF <- 'data/obsbg_anno.csv'
  .hvP <- 'hvs/4axes50pts3'
  .npts <- 50
  .axes <- c('pct_tree','pct_bare', 'ndvi','dist2forest')
  .parMethod <- 'mc'
} else {
  .pd <- getwd()
  .annoPF <- ag$dat
  .hvP <- ag$hvP
  .npts <- as.integer(ag$npts)
  .axes <- trimws(unlist(strsplit(ag$axes,',')))
  .parMethod <- ifelse(is.null(ag$parMethod),'none',ag$parMethod) #need to set value b/c NULL will fail in if statement
}


#TODO: save the actual number of points used, per niche (since available points can be < requested points)

#---- parameters ----#
set.seed(594)
.annoPF <- 'data/obsbg_anno.csv'
#.cores <- 7
#.axes <- c('pct_tree','pct_bare', 'ndvi','dist2forest')

hvP <- file.path(.pd,.hvP)
logPF <- file.path(hvP,'hvs_indiv.log')
volPF <- file.path(hvP,'niche_vols.csv')

message(glue('.pd is {.pd}'))

#---- functions ----#
diffmin <- function(t) round(difftime(Sys.time(), t, unit = "min"),2)

#---- load data ----#
message('Loading data, takes awhile...')
# envs <- read_csv(file.path(.pd,'envs.csv'),col_types=cols()) %>% 
#   filter(as.logical(run)) #This sets which axes are part of the hv calculation
niches <- read_csv(file.path(.pd,'niches.csv'),col_types=cols()) %>% filter(as.logical(run))
dat0 <- read_csv(file.path(.pd,.annoPF),col_types=cols())
message('Loading data complete')

#axes <- envs$label

#--filter for testing
# niches <- niches %>% filter(niche_set=='loburg-2013')
# dat0 <- dat0 %>% filter(niche_set=='loburg-2013')

message('Preparing data...')
#sub-sample each niche
datGrp <- dat0 %>% 
  filter(obs) %>%
  inner_join(niches %>% select(niche_set,niche_name),by='niche_name') %>%
  select(niche_set,niche_name,one_of(.axes)) %>%
  filter(complete.cases(.)) %>%
  mutate_at(.vars=vars(.axes),.funs=list(~as.vector(scale(.)))) %>%
  group_by(niche_set,niche_name) 

#subsample niches that have > .npts
datSamp <- datGrp %>% filter(n() > .npts) %>%sample_n(.npts) %>% ungroup()
#keep all data from niches that have <= .npts
datFull <- datGrp %>% filter(n() <= .npts) %>% ungroup()

datSamp <- bind_rows(datSamp,datFull)
message('Preparing data complete')
# datSamp %>% group_by(niche_set,niche_name) %>% summarize(num=n()) %>% View

#---- estimate individual hypervolumes ----#
dir.create(file.path(hvP,'hvs'),recursive=TRUE,showWarnings = FALSE)
#TODO: update to use e.g. indiv_vol=numeric() instead, so won't have extra row of NA values
tibble(niche_name=NA,indiv_vol=NA) %>% write_csv(volPF)

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

} else if(.parMethod=='mc') {
  message('Registering backend doMC')
  spsm(library(doMC))
  RNGkind("L'Ecuyer-CMRG")
  cores <- strtoi(Sys.getenv('SLURM_CPUS_PER_TASK', unset=1)) #for testing on hpc
  #cores <- 3 #for testing locally 

  registerDoMC(cores)
  
}

if(.parMethod %in% c('mpi','mc')) {
  `%mypar%` <- `%dopar%`
} else {
  message('No parallel method defined, running sequentially.')
  #foreach package as %do% so it is loaded even if the parallel packages are not
  `%mypar%` <- `%do%`
}

#----
#---- Estimate niches ----#
#----

message('Starting estimation of niches...')

tsTot <- Sys.time()

hvFiles <- foreach(i=icount(nrow(niches)),
                   .packages=c('hypervolume','dplyr','glue','readr'),
                   .combine='rbind') %mypar% {
  tsEnt <- Sys.time()
  niche <- niches[i,]
  
  datHv <- datSamp %>% 
    filter(niche_name==niche$niche_name) %>%
    select(one_of(.axes))

  message(glue('Estimating niche for {niche$niche_name}'))
  
  sink('/dev/null')
    hv <- try(hypervolume_gaussian(data=datHv,name=niche$niche_name,verbose=FALSE))
  sink()
  
  if(class(hv)=='try-error') {
    #TODO: write this information to a file.
    message(glue('hv for {niche$niche_name} failed due to error:'))
    message(hv) #hv is of class 'try-error'
  } else {
    filePF <- file.path(hvP,'hvs',glue('{niche$niche_name}.rds'))
    saveRDS(hv,filePF)
    
    #Calculate and save hv volume
    tibble(niche_name=hv@Name,indiv_vol=get_volume(hv)) %>%
      write_csv(volPF,append=TRUE)
  }
  
  rm(datHv)
  rm(hv)
  
  message(glue('Completed {niche$niche_name}, niche {i} of {nrow(niches)} in {diffmin(tsEnt)} minutes.'))
}

message('Niche estimation complete.')
message(glue('Total elapsed time: {diffmin(tsTot)} minutes.'))

#-- clean up niche_vol.csv table (remove NA from first row)
#TODO: don't need to do this if using e.g. volPF=numeric()
read_csv(volPF,col_types=cols()) %>% filter(!is.na(niche_name)) %>% write_csv(volPF)

message('Script complete.')

if(.parMethod=='mpi') { #seems nothing after mpi.quit() is executed, so make sure this is the last code
  #---- finalize doMPI ---
  closeCluster(cl)
  mpi.quit()
}


