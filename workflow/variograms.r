#--------#
# This script estimates variogram models using the hpc
# Run locally: 
#   scriptsP=~/projects/rsf/src/scripts
#   cd ~/projects/whitestork/results/stpp_models/huj_eobs
#   Rscript $scriptsP/variograms.r data/obs_trim.csv #run sequentially
#   Rscript $scriptsP/variograms.r data/obsbg_anno.csv -p mc -c 5 #run doMC with 5 cores
# Run on HPC:
#   slurm file:
#   scriptsP=~/projects/rsf/src/scripts
#   mpirun Rscript $scriptsP/variograms.r data/obsbg_anno.csv -p mpi
#
# TODO: add outDir parameter. set .ctmmP <- outDir if passed in o/w 'ctmm'
# TODO: write MPI_*.out files to directory .ctmmP
# TODO: in timing log, write if model has errors or warnings?
# TODO: restart functionality
#   pass in a "resume" file with niches that should be processed. make a small script that makes this file

spsm <- suppressPackageStartupMessages

spsm(library(docopt))
spsm(library(ctmm))
spsm(library(foreach))
spsm(library(glue))
spsm(library(iterators))
spsm(library(lubridate))
spsm(library(tidyverse))

select <- dplyr::select
filter <- dplyr::filter

'
Calculates and saves ctmm objects: telemetry, variogram, models.

Usage:
variograms.r <dat> [--parMethod=<parMethod>] [--cores=<cores>] 

Options:
-h --help     Show this screen.
-v --version     Show version.
-p --parMethod=<parMethod>  Either <mpi | mc>. If not passed in, script will run sequentially.
-c --cores=<cores> Use if parMethod is mc
' -> doc

#---- parameters ----#
#NOTE: script uses niches.csv to determine which niches to run

ag <- docopt(doc, version = '0.1\n')

if(interactive()) {
  #.pd <- '/Users/benc/projects/oilbirds/analysis/oilbirds'
  .pd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
  #.pd <- '~/projects/whitestork/results/stpp_models/huj_eobs_test'
  .datPF <- 'data/obs_trim.csv' # should use obs.csv in general. should put trim logic into obs.r
  .parMethod <- 'mc' #'none', 'mc'
  .cores <- 3 #used if .parMethod is mc
} else {
  .pd <- getwd()
  .datPF <- ag$dat
  .parMethod <- ifelse(is.null(ag$parMethod),'none',ag$parMethod) #need to set value b/c NULL will fail in if statement
  .cores <- as.integer(ag$cores)
}

#---- functions ----#
diffmin <- function(t) round(difftime(Sys.time(), t, unit = "min"),2)
ctmm.select_quietly <- quietly(ctmm.select)

#---- paths ----#
.ctmmout <- file.path(.pd,'ctmm')
.timelog <- file.path(.ctmmout,'timing.csv')

#---- create directories and files
dir.create(file.path(.ctmmout,'tel'),recursive=TRUE,showWarnings=FALSE)
dir.create(file.path(.ctmmout,'vg'),recursive=TRUE,showWarnings=FALSE)
dir.create(file.path(.ctmmout,'mod'),recursive=TRUE,showWarnings=FALSE)
tibble(niche_name=character(),event=character(),time=character()) %>%
  write_csv(.timelog)

#---- load data ----#
niches <- read_csv(file.path(.pd,'niches.csv'),col_types=cols()) %>% filter(as.logical(run))

dat0 <- read_csv(file.path(.pd,.datPF),col_types=cols()) %>%
  #filter(obs) %>% #use this if obsbg is loaded for some reason. should use obs.csv in general.
  filter(niche_name %in% niches$niche_name)

 # dat0 %>%
 #   group_by(niche_name) %>%
 #   summarize(min=min(timestamp),max=max(timestamp),num=n())

telDat <- dat0 %>%
  #group_by(niche_name) %>% sample_n(100) %>% ungroup() %>% #FOR TESTING ONLY!!
  arrange(niche_name,timestamp) %>%
  mutate(individual.local.identifier=niche_name) %>%
  select(niche_name,location.long=lon,location.lat=lat,timestamp,individual.local.identifier) %>%
  nest(-niche_name) %>%
  mutate(tel=map(data,~{
      as.data.frame(.) %>%
      as.telemetry %>%
      return()
  }))

#name the list column. just need to work with the list at this point
names(telDat$tel) <- telDat$niche_name
tels <- telDat$tel

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
  message('Registering backend doMC')
  spsm(library(doMC))
  RNGkind("L'Ecuyer-CMRG")
  if(!exists('.cores')) .cores <- strtoi(Sys.getenv('SLURM_CPUS_PER_TASK', unset=1)) #for testing on hpc
  
  registerDoMC(.cores)
  
  `%mypar%` <- `%dopar%`
  
} else {
  message('No parallel method defined, running sequentially.')
  #foreach package as %do% so it is loaded even if the parallel packages are not
  `%mypar%` <- `%do%`
}

tsTot <- Sys.time()

foreach(i=icount(nrow(niches)),
                   .packages=c('ctmm','dplyr','glue','readr'),
                   .combine='rbind') %mypar% {
  #i <- 1
  tsEnt <- Sys.time()
  niche <- niches[i,]
  tel <- tels[[niche$niche_name]]
  
  tibble(niche_name=niche$niche_name,event='start',time=tsEnt) %>%
    write_csv(.timelog,append=TRUE)

  vg <- variogram(tel)
  GUESS <- variogram.fit(vg, interactive= FALSE)
  
  message(glue('Estimating variogram parameters for {niche$niche_name}'))
  
  animal.mods <- ctmm.select_quietly(tel,
    CTMM = GUESS,
    method = "pHREML",
    control=list(method="pNewton")
  )

  tibble(niche_name=niche$niche_name,event='end',time=Sys.time()) %>%
    write_csv(.timelog,append=TRUE)
  
  saveRDS(tel,file.path(.ctmmout,'tel',glue('{niche$niche_name}.rds')))
  saveRDS(vg,file.path(.ctmmout,'vg',glue('{niche$niche_name}.rds')))
  saveRDS(animal.mods,file.path(.ctmmout,'mod',glue('{niche$niche_name}.rds')))
  
  message(glue('{niche$niche_name} complete in {diffmin(tsEnt)} minutes'))
  return(TRUE)
}

message(glue('Total elapsed time: {diffmin(tsTot)} minutes.'))
message('Script complete.')

if(.parMethod=='mpi') { #seems nothing after mpi.quit() is executed, so make sure this is the last code
  closeCluster(cl)
  mpi.quit()
}
