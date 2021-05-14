#!/usr/bin/env Rscript --vanilla
# chmod 744 script_template.r #Use to make executable

#TODO: Add progress messages.

# ==== Breezy setup ====

'
Calculates individual and niche set rini
sesid - can be a comma-seperated list of session ids

Usage:
rini.r <sesid> [-b] [-t] [--seed=<seed>]
rini.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run, will not save output parameters or commit to git
-b --rollback   If true, will rollback the transaction. Use for testing.
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
  
  .sesid <- c('full_hvs','full_ci')
  
} else {
  library(docopt)
  library(rprojroot)

  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .rollback <- as.logical(ag$rollback)
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

source(rd('src/funs/auto/breezy_funs.r'))

#---- Local parameters ----#
.dbPF <- '~/projects/ms1/analysis/huj_eobs/data/database.db'

#---- Initialize database ----#
db <- dbConnect(RSQLite::SQLite(), .dbPF)
invisible(assert_that(length(dbListTables(db))>0))

nichetb <- tbl(db,'niche')
ntb <- tbl(db,'niche_stats')
nsettb <- tbl(db,'niche_set_stats')

#---- Load data ----#
# dat0 <- read_csv(.datPF,col_types=cols()) %>%
#   inner_join(niches %>% select(niche_set,niche_name),by='niche_name')

#====

#---- Perform analysis ----#

dat <- ntb %>%
  inner_join(nichetb %>% select(niche_name,niche_set), by='niche_name') %>%
  filter(ses_id %in% .sesid) %>%
  inner_join(
    nsettb %>% select(niche_set_stats_id,ses_id,rep,niche_set,niche_set_vol),
    by=c('ses_id','rep','niche_set')) %>%
  mutate(rini=niche_vol/niche_set_vol) %>%
  select(niche_stats_id,niche_set_stats_id,ses_id,rep,niche_set,niche_name,niche_vol,niche_set_vol,rini) %>%
  arrange(ses_id,rep,niche_set,niche_name) %>%
  as_tibble

#Sanity checks
nses <- ntb %>% filter(ses_id %in% .sesid) %>% as_tibble %>% nrow
invisible(assert_that(nses == nrow(dat)))
invisible(assert_that(identical(sort(unique(dat$ses_id)),sort(.sesid))))
invisible(assert_that(nses == length(dat$niche_stats_id)))
invisible(assert_that(length(unique(dat$niche_stats_id)) == length(dat$niche_stats_id)))

#Load data
dbExecute(db,'PRAGMA foreign_keys=ON')
dbBegin(db)
  
sql <- glue_sql(
'update niche_stats 
set rini = $rini
where niche_stats_id = $niche_stats_id')

rs <- dbSendStatement(db, sql) #parameter names should match column names
dbBind(rs,params=dat %>% select(niche_stats_id,rini))
rows <- dbGetRowsAffected(rs)
dbClearResult(rs)

if(nrow(dat) != rows) {
  dbRollback(db)
  dbDisconnect(db)
  stop('All rows did not update correctly. Transaction rolled back. Exiting script.')
}

#---- Average RINI per niche set ----#
datnss <- dat %>%
  group_by(ses_id,niche_set_stats_id,rep,niche_set) %>%
  summarize(rini=mean(rini)) %>%
  arrange(ses_id,rep,niche_set) %>%
  ungroup

#Sanity checks
nrowset <- nsettb %>% filter(ses_id %in% .sesid) %>% as_tibble %>% nrow
invisible(assert_that(nrowset == nrow(datnss)))
invisible(assert_that(identical(sort(unique(datnss$ses_id)),sort(.sesid))))
invisible(assert_that(nrowset == length(unique(datnss$niche_set_stats_id))))
    
sql <- glue_sql(
'update niche_set_stats 
set rini = $rini
where niche_set_stats_id = $niche_set_stats_id')

rs <- dbSendStatement(db, sql) #parameter names should match column names
dbBind(rs,params=datnss %>% select(niche_set_stats_id,rini))
rows <- dbGetRowsAffected(rs)
dbClearResult(rs)

if(nrow(datnss) != rows) {
  dbRollback(db)
  dbDisconnect(db)
  stop('All rows did not update correctly. Transaction rolled back. Exiting script.')
}

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

#Handle commit
if(.rollback) {
  message(glue('Rolling back transaction.'))
  dbRollback(db)
} else {
  dbCommit(db)
}

dbDisconnect(db)
message(glue('Script complete in {diffmin(t0)} minutes'))