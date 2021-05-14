#!/usr/bin/env Rscript --vanilla
# chmod 744 script_template.r #Use to make executable

# ==== Breezy setup ====

'
Template

Usage:
bsnm_hvs.r <out> <reps> [--sesid=<sesid>] [--seed=<seed>] [--parMethod=<parMethod>] [--cores=<cores>] [--mpilogs=<mpilogs>] [-t] 
bsnm_hvs.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-r --sesid=<sesid>  Result session id. Used to link multiple scripts
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run, will not save output parameters or commit to git
-p --parMethod=<parMethod>  Either <mpi | mc>. If not passed in, script will run sequentially.
-c --cores=<cores>  The number of cores
-m --mpilogs=<mpilogs> Directory for the mpi log files
' -> doc


#---- Input Parameters ----#

isAbsolute <- function(path) {
  grepl("^(/|[A-Za-z]:|\\\\|~)", path)
}

if(interactive()) {
  library(here)

  .wd <- '~/projects/ms1/analysis/bsnm_test'
  .script <- 'src/poc/bootstrap/bootstrap_niche_metrics.r' #Currently executing script
  .seed <- 594 #Note that I used 594 as seed in hvs_indiv.r.
  .test <- TRUE
  .sesid <- 'test4'
  rd <- here
  
  .reps <- 2
  .outP <- file.path(.wd,.sesid)
  .parMethod <- NULL #'mc'
  
  .cores <- 7
} else {
  library(docopt)
  library(rprojroot)

  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .sesid <- ag$sesid
  .seed <- ag$seed
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .reps <- as.integer(ag$reps)
  .parMethod <- ag$parMethod
  .cores <- ag$cores
  
  .datPF <- ifelse(isAbsolute(ag$dat),ag$dat,file.path(.wd,ag$dat))
  .outP <- ifelse(isAbsolute(ag$out),ag$out,file.path(.wd,ag$out))
  
  ag$mpilogs <- ifelse(is.null(ag$mpilogs),'mpilogs',ag$mpilogs)
  .mpiLogP <- ifelse(isAbsolute(ag$mpilogs),ag$mpilogs,file.path(.wd,ag$mpilogs))
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed)) 

set.seed(.seed)
t0 <- Sys.time()

options(rgl.useNULL = TRUE) #hypervolume loads rgl, this makes sure it doesn't open x11 session

source(rd('src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(foreach)
    library(hypervolume)
    library(iterators)
  }))

source(rd('src/funs/auto/breezy_funs.r'))
source(rd('src/funs/hv.r'))

#---- Local parameters ----#
.outPF <- file.path(.wd,.sesid,'niche_set_stats.csv')
.dfUnion <- 3

#---- Files and directories ----#
dir.create(dirname(.outPF),showWarnings=FALSE,recursive=TRUE)

# Niche set-level stats
tibble(ses_id=character(),
       rep=numeric(),
       niche_set=character(),
       niche_set_vol=numeric(),
       minutes=numeric()) %>% write_csv(.outPF)

#---- Load control files ----#
nsets <- read_csv(file.path(.wd,'ctfs/niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'ctfs/niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')
#====

# ==== start cluster and register backend ====
if(is.null(.parMethod)) {
  message('No parallel method defined, running sequentially.')
  #foreach package as %do% so it is loaded even if the parallel packages are not
  `%mypar%` <- `%do%`
} else if(.parMethod=='mpi') {
  message('Registering backend doMPI')
  library(doMPI)
  
  dir.create(.mpiLogP,showWarnings=FALSE,recursive=TRUE)
  #start the cluster. number of tasks, etc. are defined by slurm in the init script.
  message('Starting mpi cluster.')
  cl <- startMPIcluster(verbose=TRUE,logdir=.mpiLogP)
  registerDoMPI(cl)
  setRngDoMPI(cl) #set each worker to receive a different stream of random numbers
  
  `%mypar%` <- `%dopar%`
  
} else if(.parMethod=='mc') {
  #.cores <- strtoi(Sys.getenv('SLURM_CPUS_PER_TASK', unset=1)) #for testing on hpc
  message(glue('Registering backend doMC with {.cores} cores'))
  library(doMC)
  RNGkind("L'Ecuyer-CMRG")
  
  registerDoMC(.cores)
  
  `%mypar%` <- `%dopar%`
  
} else {
  stop('Invalid parallel method')
}
# ====

#==== Estimate Population niches ====
message('Estimating niche set hypervolumes')

foreach(i=icount(nrow(nsets))) %:%
  foreach(j=icount(.reps)) %mypar% {
    #i <- 1; j <- 1
    tNs <- Sys.time()
    nset <- nsets[i,]$niche_set
    
    message(glue('Starting niche set {nset} rep {j}'))
    
    hvsDf <- niches %>% filter(niche_set==nset) %>%
      mutate(hv=map(niche_name,~{
        file.path(.outP,nset,glue('rep{j}_{.x}.rds')) %>% readRDS
      }))
    
    sink('/dev/null')
    hvNset <- unionHvs(hvsDf$hv,.dfUnion) #This is a local function
    sink()
    
    tibble(ses_id=.sesid,
           rep=j,
           niche_set=nset,
           niche_set_vol=get_volume(hvNset),
           minutes=as.numeric(diffmin(tNs))) %>% 
      write_csv(.outPF,append=TRUE)
    
    message(glue('Total time to estimate niche set {nset} rep {j}: {diffmin(tNs)} minutes.'))
    return(TRUE)
  } -> status
#====

#==== Breezy finalize script ====

if(!.test) {
  suppressWarnings(
    suppressPackageStartupMessages({
      library(git2r)
      library(uuid)
    }))

  .runid <- UUIDgenerate()
  .parPF <- file.path(.wd,"run_params.csv")
  
  #Update repo and pull out commit sha
  repo <- repository(rd('src'))
  
  rstat <- status(repo)
  if(length(rstat$staged) + 
     length(rstat$unstaged) + 
     length(rstat$untracked) > 0) {
    add(repo,'.')
    commit(repo, glue('script auto update. runid: {.runid}'))
  }
  
  
  .git_sha <- sha(repository_head(repo))
  
  #Save all parameters to csv for reproducibility
  #TODO: write this to a workflow database instead
  saveParams(.parPF)
}

message(glue('Script complete in {diffmin(t0)} minutes'))
#====

if(!is.null(.parMethod) && .parMethod=='mpi') { #seems nothing after mpi.quit() is executed, so make sure this is the last code
  closeCluster(cl)
  mpi.quit()
}

