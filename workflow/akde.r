#--------#
# This script estimates akde in parallel
#   assumes that telemetry and variogram model objects exist
# Run locally: 
#   scriptsP=~/projects/rsf/src/scripts
#   cd ~/projects/whitestork/results/stpp_models/huj_eobs
#   Rscript $scriptsP/akde.r #run sequentially
#   Rscript $scriptsP/akde.r -p mc -c 6 #run using doMc
# Run on HPC:
#   slurm file:
#   scriptsP=~/projects/rsf/src/scripts
#   mpirun Rscript $scriptsP/akde.r -p mpi

spsm <- suppressPackageStartupMessages

spsm(library(ctmm))
spsm(library(docopt))
spsm(library(foreach))
spsm(library(glue))
spsm(library(iterators))
spsm(library(lubridate))
spsm(library(tidyverse))

'
Calculates population hypervolumes based on individual hypervolumes.

Usage:
nmds_hv [--parMethod=<parMethod>] [--cores=<cores>] [--out=<out>]
nmds_hv (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-p --parMethod=<parMethod>  Either <mpi | mc>. If not passed in, script will run sequentially.
-c --cores=<cores> Use if parMethod is mc
-o --out=<out> Output directory. Defaults to ./ctmm/akde
' -> doc

#---- parameters ----#
#NOTE: script uses niche_sets.csv and niches.csv to determine which niches to run

ag <- docopt(doc, version = '0.1\n')

if(interactive()) {
  .pd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
  .hvP <- 'hvs/5axes2000pts1'
  .npts <- 20
  .parMethod <- 'none' #'none', 'mc'
  .cores <- 3 #used if .parMethod is mc
  .out <- 'ctmm/akde'
} else {
  .pd <- getwd()
  .hvP <- ag$hvP
  .npts <- as.integer(ag$npts)
  .parMethod <- ifelse(is.null(ag$parMethod),'none',ag$parMethod) #need to set value b/c NULL will fail in if statement
  .cores <- as.integer(ag$cores)
  .out <- ag$out
}

.ctmmP <- file.path(.pd,'ctmm') #this is where tel, mod directories are located
.outP <- file.path(.pd,ifelse(is.null(.out),'ctmm/akde',.out)) #this is where output and slurm logs are saved
.tlogPF <- file.path(.outP,'timing.csv')

#---- functions ----#
diffmin <- function(t) round(difftime(Sys.time(), t, unit = "min"),2)
akde_q <- quietly(akde)

#---- load data ----#
niches <- read_csv(file.path(.pd,'niches.csv'),col_types=cols()) %>% filter(as.logical(run))

#---- initialize directories and files ----#
dir.create(.outP,recursive=TRUE,showWarnings=FALSE)
tibble(entity_name=character(),event=character(),time=character()) %>%
  write_csv(.tlogPF)

#----
#---- start cluster and register backend ----
#---- 
if(.parMethod=='mpi') {
  message('Registering backend doMPI')
  spsm(library(doMPI))
  
  #start the cluster. number of tasks, etc. are defined by slurm in the init script.
  message('Starting mpi cluster.')
  
  cl <- startMPIcluster(verbose=TRUE,logdir=.outP)
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
  
  tibble(entity_name=niche$niche_name,event='start',time=tsEnt) %>%
    write_csv(.tlogPF,append=TRUE)
  
  tel <- readRDS(file.path(.ctmmP,'tel',glue('{niche$niche_name}.rds')))
  mod <- readRDS(file.path(.ctmmP,'mod',glue('{niche$niche_name}.rds'))) %>% pluck('result')
  
  #First get the best fit model if more than 1 have been fit
  if(class(mod) == "ctmm") {
    mod <- mod
  } else {
    mod <- mod[[1]]
  }
  
  #calculate the akde based on the best fit model
  message(glue('Estimating akde for {niche$niche_name}'))
  animal.akde <- akde_q(tel, mod, res = 50)
  saveRDS(animal.akde,file.path(.outP,glue('{niche$niche_name}.rds')))

  tibble(entity_name=niche$niche_name,event='end',time=Sys.time()) %>%
    write_csv(.tlogPF,append=TRUE)
  
  message(glue('{niche$niche_name} complete in {diffmin(tsEnt)} minutes'))
  return(TRUE)
}

message(glue('Total elapsed time: {diffmin(tsTot)} minutes.'))
message('Script complete.')

if(.parMethod=='mpi') { #seems nothing after mpi.quit() is executed, so make sure this is the last code
  closeCluster(cl)
  mpi.quit()
}

# plot(tel,
#      UD=animal.akde,
#      col.grid = NA,
#      family = "serif",
#      pch = 20,
#      cex = 0.2,
#      col.DF = "#669543",
#      labels=FALSE)