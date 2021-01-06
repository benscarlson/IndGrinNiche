#!/usr/bin/env Rscript

'
Calculates repeatability for a set of traits
Currently traits defined by an ssf model and specialization
Saves to analysis database

Usage:
repeatability.r <hvjob> <mod> [--seed=<seed>] [-t]
repeatability.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run
' -> doc

#TODO: should pull out .rpt_f from a csv file or table, given .rpt_mod
#TODO: should have a more dynamic way to pull in traits. Difficult though
# because traits come from different sources.

#---- Input Parameters ----#
if(interactive()) {
  library(here)
  
  .wd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
  #.script <- 'src/workflow_repeatability.r' #Should not need in interactive mode
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .hvjob <- '5axes2000pts1'
  .mod <- 'mod4'
  .outPF <- file.path(.wd,'figures/myfig.png')
  
} else {
  library(docopt)
  library(rprojroot)
  
  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .hvjob <- ag$hvjob
  .mod <- ag$mod
  .outPF <- ag$out
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(.seed)
t0 <- Sys.time()

source(rd('src/startup.r'))

spsm(library(DBI))
spsm(library(lme4))
spsm(library(rptR))
spsm(library(RSQLite))

source(rd('src/funs/funs.r'))
source(rd('src/funs/tidy_rpt.r'))

#---- Parameters ----#
.dbPF <- file.path(.wd,"data/database.db")
.rpt_mod <- 'simple'
.rpt_f <- formula('value ~ (1 | individual_id)') #Should pull this out of table

#---- Load data ----#

db <- dbConnect(RSQLite::SQLite(), .dbPF)
indst <- tbl(db,'indiv_stats') %>% filter(hv_job==.hvjob)
msum <- tbl(db,'model_summary') %>% filter(mod_name==.mod)

nsets <- read_csv(file.path(.wd,'niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

#---- Perform analysis ----#

#Get selection coefficients
est <- msum %>% as_tibble %>%
  filter(component=='estimate') %>%  
  select(-model_summary_id,-mod_name,-component) %>%
  spread(term,value)

#Note, not using population or niche set, individual is the only grouping var
dat <- indst %>% select(niche_name,rini) %>% as_tibble %>% 
  inner_join(niches %>% select(niche_name,individual_id),by='niche_name') %>%
  mutate(spec=1-rini) %>%
  left_join(est,by='niche_name') %>%
  select(-c(niche_name,rini)) %>%
  gather(key=trait,value=value,-individual_id)


#--- Calculate R --#

t1 <- Sys.time()
rptdf <- dat %>%
  nest(data=-trait) %>%
  mutate(rpt=map(data, 
    ~rpt(.rpt_f, 
      grname = "individual_id", data = .x, 
      datatype = "Gaussian", nboot = 1000, npermut = 0)))
diffmin(t1)

rptdat <- rptdf %>% 
  mutate(rpt_tidy = map(rpt,tidy.rpt)) %>%
  unnest(rpt_tidy) %>%
  select(-c(data,rpt,group)) %>%
  rename(conf_low=conf.low,conf_high=conf.high) %>%
  mutate(mod=.mod,hvjob=.hvjob,rpt_mod=.rpt_mod)

#-- Delete any existing values
sql <- glue_sql('delete from rpt 
  where mod={.mod} and hvjob={.hvjob} and rpt_mod={.rpt_mod}',.con=db)

dbBegin(db)
dbExecute(db,sql)

rptdat %>% dbAppendTable(db,'rpt',.)

#---- Finalize script ----#

if(!.test) {
  spsm(library(git2r))
  spsm(library(uuid))
  
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

dbCommit(db)
dbDisconnect(db)

message(glue('Script complete in {diffmin(t0)} minutes'))
