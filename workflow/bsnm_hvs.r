#!/usr/bin/env Rscript --vanilla
# chmod 744 script_template.r #Use to make executable

#TODO: change col names to match database
#TODO: 

# ==== Breezy setup ====

'
Template

Usage:
bsnm_hvs.r <dat> <out> <reps> [--bmode=<bmode>] [--sesid=<sesid>] [--seed=<seed>] [--parMethod=<parMethod>] [--cores=<cores>] [--mpilogs=<mpilogs>] [-t] 
bsnm_hvs.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-b --bmode=<bmode>  Bootstrap sampling mode. null resamples from full niche set. ci resamples from each niche
-r --sesid=<sesid>  Id that uniquely identifies a script run
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

  .wd <- '~/projects/ms1/analysis/bsnm'
  .seed <- NULL
  .test <- TRUE
  .sesid <- 'full_bg_buf'
  rd <- here
  
  .reps <- 2
  .bmode <- 'ci'
  
  #.datPF <- '~/projects/ms1/data/derived/obs_anno_100.csv'
  .datPF <- file.path(.wd,'data/full_bg_buf_2k.csv')
  .outP <- file.path(.wd,.sesid)
  
  .parMethod <- NULL
  #.cores <- 7
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
  
  #.npts <- as.integer(ag$npts)
  .reps <- as.integer(ag$reps)
  #.axes <- trimws(unlist(strsplit(ag$axes,',')))
  .bmode <- ag$bmode
  .parMethod <- ag$parMethod
  .cores <- ag$cores
  
  .datPF <- ifelse(isAbsolute(ag$dat),ag$dat,file.path(.wd,ag$dat))
  .outP <- ifelse(isAbsolute(ag$out),ag$out,file.path(.wd,ag$out))
  
  ag$mpilogs <- ifelse(is.null(ag$mpilogs),'mpilogs',ag$mpilogs)
  .mpiLogP <- ifelse(isAbsolute(ag$mpilogs),ag$mpilogs,file.path(.wd,ag$mpilogs))
}

#---- Initialize Environment ----#
# Then, I need to permute the ids of the subsample to calculate the null
# TODO: I should so a distribution where I pick different subsamples but don't permute!
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
source(rd('src/funs/themes.r'))
source(rd('src/funs/hv.r'))

theme_set(theme_eda)

#---- Local parameters ----#
.outPF <- file.path(.wd,.sesid,'niche_stats.csv')
.statusPF <- file.path(.wd,.sesid,'status.csv')  
#---- Files and directories ----#

dir.create(.outP,showWarnings=FALSE,recursive=TRUE)
dir.create(dirname(.outPF),showWarnings=FALSE,recursive=TRUE)

# Niche-level stats
tibble(ses_id=character(), 
       rep=numeric(),
       niche_set=character(),
       niche_name=character(),
       niche_vol=numeric(),
       minutes=numeric()) %>% write_csv(.outPF)

#---- Load control files ----#
nsets <- read_csv(file.path(.wd,'ctfs/niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'ctfs/niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set') %>%
  mutate(niche_id=row_number()) #need a niche_id to make matrix for clust_w

#---- Initialize database ----#

#---- Load data ----#
message('Loading data...')
dat0 <- read_csv(.datPF,col_types=cols()) %>%
  inner_join(niches %>% select(niche_set,niche_name),by=c('niche_set','niche_name'))
#====

#need to know the number of points to select for each niche, since it can be less than .npts
#although resampling with replacement from the population, maybe this doesn't matter
niches <- niches %>%
  inner_join(
    dat0 %>% group_by(niche_name) %>% summarize(npts=n()),by='niche_name')

#==== start cluster and register backend ====
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
#====

#==== Estimate hypervolumes ====
foreach(i=icount(nrow(niches)),.combine='rbind') %:%
foreach(j=icount(.reps),.combine='rbind') %mypar% {
  #i<-1; j<-1
  tsNiche <- Sys.time()
  niche <- niches[i,]

  message(glue('Starting niche {niche$niche_name} rep {j}'))
  hvPF <- file.path(.outP,niche$niche_set,glue('rep{j}_{niche$niche_name}.rds'))
  dir.create(dirname(hvPF),showWarnings=FALSE,recursive=TRUE)
  
  # bmode==null. Sample with replacement from the full niche set
  # bmode==ci. Sample with replacement from the niche
  # But don't resample on first repetition
  
  #In rare cases, bandwidth can be zero, causing the script to fail.
  # Try resample three times before giving up.
  for(a in 1:3) {
    if(j == 1) {
      message(glue('Niche {niche$niche_name} rep {j} does not have random selection'))
      nicheDat <- dat0 %>% filter(niche_name==niche$niche_name)
    } else if(.bmode=='null') {
      message(glue('Niche {niche$niche_name} rep {j} sampling from niche set (null model)'))
      nicheDat <- dat0 %>% #sample points with replacement from the niche set
        filter(niche_set==niche$niche_set) %>%
        sample_n(size=niche$npts,replace=TRUE)
    } else if(.bmode=='ci') {
      message(glue('Niche {niche$niche_name} rep {j} sampling from niche (ci)'))
      nicheDat <- dat0 %>% #sample with replacement from each niche
        filter(niche_name==niche$niche_name) %>% 
        sample_n(size=niche$npts,replace=TRUE)
    }
    
    hvDat <- nicheDat %>% select(-c(niche_set,niche_name))
    
    bw <- estimate_bandwidth(hvDat)

    if(all(bw != 0)) break
    message('Bandwidth=0, performing resampling procedure again.')
  }
  
  if(any(bw==0)) {
    message('Failed because bandwidth=0')
    return('Failed. Bandwidth=0')
  }
  
  sink('/dev/null')
  hv <- hypervolume_gaussian(hvDat, name=niche$niche_name,kde.bandwidth=bw,verbose=FALSE)
  sink()
  
  message(glue('Saving hv to {hvPF}'))
  hv %>% saveRDS(hvPF)
  
  tibble(ses_id=.sesid,
         rep=j,
         niche_set=niche$niche_set,
         niche_name=niche$niche_name,
         niche_vol=get_volume(hv),
         minutes=diffmin(tsNiche)) %>% 
    write_csv(.outPF,append=TRUE)
  
  message(glue('Completed rep {j}, niche {niche$niche_name} in {diffmin(tsNiche)} minutes.'))
  
  return('Success')
} -> status

status %>% 
  as_tibble(.name_repair = 'minimal') %>%
  rename(status=1) %>% 
  write_csv(.statusPF)
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

