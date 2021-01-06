#!/usr/bin/env Rscript --vanilla

# ==== Breezy setup ====

'
Calculates 95% CI from distributions given in sesid
sesid can be a list of sesids

Usage:
metric_ci.r <sesid> [--seed=<seed>] [-t] 
metric_ci.r (-h | --help)

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

  .wd <- '~/projects/project_template/analysis'
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .sesid <- c('full_hvs','full_bg_buf','full_ci')
  
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

source(rd('src/funs/breezy_funs.r'))

#---- Local parameters ----#
.dbPF <- '~/projects/whitestork/results/stpp_models/huj_eobs/data/database.db'

#---- Load control files ----#

#---- Initialize database ----#
db <- dbConnect(RSQLite::SQLite(), .dbPF)
invisible(assert_that(length(dbListTables(db))>0))

nsettb <- tbl(db,'niche_set_stats')
citb <- tbl(db,'metric_ci')
#---- Load data ----#

#====

#---- Perform analysis ----#

#Make sure there are not already results for this distribution and statistic
r <- citb %>% filter(ses_id %in% .sesid) %>% as_tibble %>% nrow
invisible(assert_that(r==0))

message('Calculating 95% confidence intervals...')
#Rep=1 has the unrandomized values
#There should be 100 randomized values per niche set for each ses id
dat <- nsettb %>% 
  filter(ses_id %in% .sesid & rep != 1) %>%
  select(ses_id,niche_set,spec,nestedness,clust_w) %>%
  as_tibble %>%
  pivot_longer(cols=c(spec,nestedness,clust_w),names_to='metric') %>%
  group_by(ses_id,niche_set,metric) %>%
  summarize(
    ci_low=quantile(value,0.025),
    ci_high=quantile(value,0.975)) %>%
  arrange(ses_id,niche_set,desc(metric))
  

#---- Save output ---#
message('Saving to database...')
dat %>% 
  dbAppendTable(db, "metric_ci", .)

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