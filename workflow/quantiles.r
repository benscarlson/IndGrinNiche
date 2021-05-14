#!/usr/bin/env Rscript --vanilla

# ==== Breezy setup ====

'
Calculates quantiles from metrics in stat given distributions in sesid
sesid can be a list of sesids
stat should be only one sesid

Usage:
quantiles.r <sesid> <stat> [--seed=<seed>] [-t] 
quantiles.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run, will not save output parameters or commit to git
' -> doc

isAbsolute <- function(path) {
  grepl("^(/|[A-Za-z]:|\\\\|~)", path)
}

#---- Input Parameters ----#
if(interactive()) {
  library(here)

  .wd <- '~/projects/ms1/analysis/rev2/null3_full'
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .sesid <- c('null3') #null3_full
  .stat <- '5axes2000pts1'
  
} else {
  library(docopt)
  library(rprojroot)

  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .sesid <- trimws(unlist(strsplit(ag$sesid,',')))
  .stat <- ag$stat
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(.seed)
t0 <- Sys.time()

source(rd('src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(DBI)
    library(RSQLite)
  }))

source(rd('src/funs/auto/breezy_funs.r'))

#---- Local parameters ----#
.dbPF <- '~/projects/ms1/analysis/huj_eobs/data/database.db'

#---- Load control files ----#

#---- Initialize database ----#
db <- dbConnect(RSQLite::SQLite(), .dbPF)
invisible(assert_that(length(dbListTables(db))>0))

nsettb <- tbl(db,'niche_set_stats')
qtb <- tbl(db,'metric_quantile')
#---- Load data ----#

#====

#---- Perform analysis ----#

#Make sure there are not already results for this distribution and statistic
r <- qtb %>% filter(dist_ses_id %in% .sesid & stat_ses_id == .stat) %>% as_tibble %>% nrow
invisible(assert_that(r==0))

message('Calculating quantiles...')
#Rep=1 has the unrandomized values
#There should be 100 randomized values per niche set for each ses id
dat <- nsettb %>% 
  filter(ses_id %in% .sesid & rep != 1) %>%
  select(dist_ses_id=ses_id,niche_set,spec,nestedness,clust_w) %>%
  as_tibble %>%
  pivot_longer(cols=c(spec,nestedness,clust_w),names_to='metric') %>%
  arrange(dist_ses_id,niche_set,desc(metric))

#This has the observed statistic as calculated from the data
#The observed statistic should be a single sesid. not sure if multiple sesids here even makes sense
stdat <- nsettb %>%
  filter(ses_id == .stat & (is.na(rep) | rep==1)) %>%
  select(stat_ses_id=ses_id,niche_set,spec,nestedness,clust_w) %>%
  as_tibble %>%
  pivot_longer(cols=c(spec,nestedness,clust_w),names_to='metric') %>%
  arrange(niche_set,desc(metric))

#https://stat.ethz.ch/pipermail/r-help/2012-March/305368.html
#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/ecdf
qdat <- dat %>%
  nest(data=value) %>%
  inner_join(stdat,by=c('niche_set','metric')) %>%
  mutate(quantile=map2_dbl(data,value,~{ecdf(.x$value)(.y)})) %>%
  select(niche_set,metric,dist_ses_id,stat_ses_id,quantile) %>%
  pivot_wider(names_from=metric,values_from=quantile,names_prefix='q_')
  
#---- Save output ---#
message('Saving to database...')
qdat %>% 
  dbAppendTable(db, "metric_quantile", .)


#---- Finalize script ----#

if(!.test) {
  library(git2r)
  library(uuid)
  
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

dbDisconnect(db)

message(glue('Script complete in {diffmin(t0)} minutes'))