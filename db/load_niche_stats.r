#!/usr/bin/env Rscript --vanilla
# chmod 744 script_template.r #Use to make executable

'
Loads csv output from hpc to the database.

Usage:
load_niche_stats.r <sesid> [-t] [--seed=<seed>]
load_niche_stats.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run, will not save output parameters or commit to git
' -> doc

#---- Input Parameters ----#
if(interactive()) {
  library(here)

  .wd <- '/Users/benc/projects/ms1/analysis/bsnm'
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .sesid <- 'full_hvs'
} else {
  library(docopt)
  library(rprojroot)

  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .sesid <- ag$sesid
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
source(rd('src/funs/themes.r'))
theme_set(theme_eda)

#---- Local parameters ----#
.dbPF <- '~/projects/whitestork/results/stpp_models/huj_eobs/data/database.db'
.datPF <- file.path(.wd,.sesid,'niche_stats.csv')

#---- Load control files ----#

#---- Initialize database ----#
db <- dbConnect(RSQLite::SQLite(), .dbPF)
invisible(assert_that(length(dbListTables(db))>0))

#---- Load data ----#
dat0 <- read_csv(.datPF,col_types=cols())

#---- Perform analysis ----#

nrows <- 'select count(*) from niche_stats where ses_id = {.sesid}' %>%
  glue_sql(.con=db) %>%
  dbGetQuery(db,.)

#Make sure there is no data for the session
invisible(assert_that(nrows==0,
  msg=glue('Data already exists for session {.sesid}')))

#Insert the data
dat0 %>%
  mutate(niche_stats_id=NA) %>%
  select(niche_stats_id,ses_id,rep,niche_name,niche_vol) %>%
  dbAppendTable(db, "niche_stats", .)

#---- Save output ---#

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

dbDisconnect(db)

message(glue('Script complete in {diffmin(t0)} minutes'))