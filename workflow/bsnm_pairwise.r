#!/usr/bin/env Rscript --vanilla
# chmod 744 script_template.r #Use to make executable

#TODO: update column  names to match database.

# ==== Breezy setup ====

'
Template

Usage:
bsnm_pairwise.r <out> <reps> [--sesid=<sesid>] [--seed=<seed>] [--parMethod=<parMethod>] [--cores=<cores>] [--mpilogs=<mpilogs>] [-t] 
bsnm_pairwise.r (-h | --help)

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
  .seed <- NULL
  .test <- TRUE
  .sesid <- 'test_ci'
  rd <- here
  
  .reps <- 2
  .outP <- file.path(.wd,.sesid)
  .parMethod <- NULL #'mc'
  #.cores <- 4
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

source(rd('src/funs/breezy_funs.r'))
source(rd('src/funs/hv.r'))

#---- Local parameters ----#
.outPF <- file.path(.wd,.sesid,'pairwise.csv')
.dfUnion <- 3
.dfIntr <- 1

#---- Files and directories ----#
dir.create(dirname(.outPF),showWarnings=FALSE,recursive=TRUE)

# Pairwise-level stats
tibble(ses_id=character(),
       rep=numeric(),
       niche_name1=character(),
       niche_name2=character(),
       hv_intr_vol=numeric(),
       hv_union_vol=numeric(),
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

# ==== Estimate pairwise components ====

pairs <- niches %>% 
  select(niche_set,niche_name) %>%
  arrange(niche_set,niche_name) %>% #make sure name pairs will be in order
  nest(niche_names=-niche_set) %>%
  mutate(pairs=map(niche_names,~{
    .x$niche_name %>%
      combn(2,simplify=TRUE) %>%
      t %>%
      as_tibble(.name_repair='minimal') %>%
      set_names(c('niche_name1','niche_name2'))
  })) %>%
  select(-niche_names) %>%
  unnest(pairs)

message(glue('There are a total of {nrow(pairs)} * {.reps} = {format(nrow(pairs) * .reps,big.mark=",")} pairs'))
message('Starting estimation of pairwise components...')
tsTot <- Sys.time()

foreach(i=icount(nrow(pairs))) %:%
  foreach(j=icount(.reps)) %mypar% {
    #i <- 1; j <- 1
    tsPair <- Sys.time()
    pair <- pairs[i,][,c('niche_name1','niche_name2')]
    nset <- pairs[i,]$niche_set
    
    message(glue('Starting {paste(pair,collapse=" & ")}, rep {j}, {nset}'))
  
    #Load the hypervolumes
    hv1 <- file.path(.outP,nset,glue('rep{j}_{pair$niche_name1}.rds')) %>% readRDS
    hv2 <- file.path(.outP,nset,glue('rep{j}_{pair$niche_name2}.rds')) %>% readRDS
  
    #Calculate pairwise metrics
    sink('/dev/null')
    hv_intr_vol <- hypervolume_set(hv1,hv2,verbose=FALSE,check.memory=FALSE, distance.factor=.dfIntr) %>%
      pluck('HVList') %>% pluck('Intersection') %>% get_volume
  
    hv_union_vol <- hypervolume_set(hv1,hv2,verbose=FALSE,check.memory=FALSE, distance.factor=.dfUnion) %>%
      pluck('HVList') %>% pluck('Union') %>% get_volume
    sink()
  
    tibble(ses_id=.sesid,
           rep=j,
           niche_name1=as.character(pair[1]),
           niche_name2=as.character(pair[2]),
           hv_intr_vol=hv_intr_vol,
           hv_union_vol=hv_union_vol,
           minutes=as.numeric(diffmin(tsPair))) %>%
      write_csv(.outPF,append=TRUE)
  
    message(glue('Completed {paste(pair,collapse=" & ")}, rep {j}, {nset} in {diffmin(tsPair)} minutes.'))
    return(TRUE)
} -> status

message(glue('Total time for pairwise components: {diffmin(tsTot)} minutes.'))
# ====

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

