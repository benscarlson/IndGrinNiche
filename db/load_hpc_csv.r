#!/usr/bin/env Rscript --vanilla

'
Loads csv output from hpc to the database.
.import command in sqlite3 is very rudimentary so this is better

Usage:
load_hpc_csv.r <dat> <sesid> <table> [-t] [--seed=<seed>]
load_hpc_csv.r (-h | --help)

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

  .wd <- '/Users/benc/projects/ms1/analysis/bsnm'
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .sesid <- 'full_hvs'
  .datPF <- file.path(.wd,.sesid,'pairwise.csv')
  .table <- 'pairwise'
} else {
  library(docopt)
  library(rprojroot)

  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .datPF <- ifelse(isAbsolute(ag$dat),ag$out,file.path(.wd,ag$dat))
  .sesid <- ag$sesid
  .table <- ag$table
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

#---- Load data ----#
dat0 <- read_csv(.datPF,col_types=cols())

#---- Perform analysis ----#

nrows <- 'select count(*) from {.table} where ses_id = {.sesid}' %>%
  glue_sql(.con=db) %>%
  dbGetQuery(db,.)

#Make sure there is no data for the session
invisible(assert_that(nrows==0,
  msg=glue('Data already exists for session {.sesid}')))

#Insert the data

pk <- glue('{.table}_id')

#TODO: should pass this in as a parameter
if(.table=='niche_stats') {
  cols <- c(pk,'ses_id','rep','niche_name','niche_vol')
} else if(.table=='niche_set_stats') {
  cols <- c(pk,'ses_id','rep','niche_set','niche_set_vol')
} else if(.table=='pairwise') {
  cols <- c(pk, 'ses_id', 'rep', 'niche_name1', 'niche_name2', 'hv_intr_vol', 'hv_union_vol')
} else {
  stop('Invalid table.')
}

message(glue('Attempting to insert {nrow(dat0)} rows'))

dbBegin(db)

nrows <- dat0 %>%
  mutate(!!glue('{.table}_id') := NA) %>%
  select(!!cols) %>%
  dbAppendTable(db, .table, .)

if(nrow(dat0)==nrows) {
  message(glue('Successfully  Inserted {nrows} rows'))
  dbCommit(db)
} else {
  message(glue('Failed to insert {nrow(dat0)}'))
  dbRollback(db)
}

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