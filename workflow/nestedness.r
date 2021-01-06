#!/usr/bin/env Rscript --vanilla

# ==== Breezy setup ====

'
Calculates pairwise and niche set nestedness
sesid - can be a comma-seperated list of session ids

Usage:
nestedness.r <sesid> [-t] [--seed=<seed>]
nestedness.r (-h | --help)

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
  
  .sesid <- c('full_hvs','full_ci')
  
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

#---- Initialize database ----#
db <- dbConnect(RSQLite::SQLite(), .dbPF)
invisible(assert_that(length(dbListTables(db))>0))

nichetb <- tbl(db,'niche')
ntb <- tbl(db,'niche_stats')
nsettb <- tbl(db,'niche_set_stats')
pwtb <- tbl(db,'pairwise')


#====

message(glue('Calculating pairwise and nicheset nestedness for session ids: {.sesid}'))

#---- Perform analysis ----#
#join twice to get volumns for both niches
dat <- pwtb %>%
  select(pairwise_id,ses_id,rep,niche_name1,niche_name2,hv_intr_vol) %>%
  filter(ses_id %in% .sesid) %>%
  inner_join(ntb %>% select(ses_id,rep,niche_name,niche_vol),
    by=c('niche_name1'='niche_name','rep','ses_id')) %>%
  rename(vol1=niche_vol) %>%
  inner_join(ntb %>% select(ses_id,rep,niche_name,niche_vol),
            by=c('niche_name2'='niche_name','rep','ses_id')) %>%
  rename(vol2=niche_vol) %>%
  inner_join(nichetb %>% select(niche_name,niche_set),
    by=c('niche_name1'='niche_name')) %>%
  as_tibble

message('Calculating pairwise nestedness')

#Calculate nestedness
nestdf <- dat %>%
  mutate(nestedness=pmap_dbl(list(hv_intr_vol,vol1,vol2),
   function(hv_intr_vol,vol1,vol2) {
     hv_intr_vol/min(vol1,vol2)
   }))

#Sanity checks
nses <- pwtb %>% filter(ses_id %in% .sesid) %>% as_tibble %>% nrow
invisible(assert_that(nses == nrow(nestdf)))
invisible(assert_that(identical(sort(unique(nestdf$ses_id)),sort(.sesid))))
invisible(assert_that(nses == length(nestdf$pairwise_id)))
invisible(assert_that(length(unique(nestdf$pairwise_id)) == length(nestdf$pairwise_id)))

#Load data
invisible(dbExecute(db,'PRAGMA foreign_keys=ON'))
dbBegin(db)
  
sql <- glue_sql(
'update pairwise 
set nestedness = $nestedness
where pairwise_id = $pairwise_id')

message(glue('Inserting {nrow(dat)} rows'))
rs <- dbSendStatement(db, sql) #parameter names should match column names
dbBind(rs,params=nestdf %>% select(pairwise_id,nestedness))
rows <- dbGetRowsAffected(rs)
dbClearResult(rs)

if(nrow(dat) != rows) {
  dbRollback(db)
  dbDisconnect(db)
  stop('All rows did not update correctly. Transaction rolled back. Exiting script.')
} else {
  message(glue('Successfully inserted {rows} rows'))
}

#---- Average RINI per niche set ----#
message('Calculating nicheset nestedness')

datnss <- nestdf %>%
  group_by(ses_id,rep,niche_set) %>%
  summarize(nestedness=mean(nestedness)) %>%
  arrange(ses_id,rep,niche_set) %>%
  ungroup %>%
  inner_join(
    nsettb %>% as_tibble %>% select(niche_set_stats_id,ses_id,rep,niche_set),
    by=c('ses_id','rep','niche_set')) %>%
  select(niche_set_stats_id,everything())

#Sanity checks
nrowset <- nsettb %>% filter(ses_id %in% .sesid) %>% as_tibble %>% nrow
invisible(assert_that(nrowset == nrow(datnss)))
invisible(assert_that(identical(sort(unique(datnss$ses_id)),sort(.sesid))))
invisible(assert_that(nrowset == length(datnss$niche_set_stats_id)))
invisible(assert_that(length(unique(datnss$niche_set_stats_id)) == length(datnss$niche_set_stats_id)))

sql <- glue_sql(
'update niche_set_stats 
set nestedness = $nestedness
where niche_set_stats_id = $niche_set_stats_id')

message(glue('Inserting {nrow(datnss)} rows'))

rs <- dbSendStatement(db, sql) #parameter names should match column names
dbBind(rs,params=datnss %>% select(niche_set_stats_id,nestedness))
rows <- dbGetRowsAffected(rs)
dbClearResult(rs)

if(nrow(datnss) != rows) {
  dbRollback(db)
  dbDisconnect(db)
  stop('All rows did not update correctly. Transaction rolled back. Exiting script.')
} else {
  message(glue('Successfully inserted {rows} rows'))
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

if(.test) {
  message('Rolling back transaction because this is a test run.')
  dbRollback(db)
} else {
  dbCommit(db)
}

dbDisconnect(db)
message(glue('Script complete in {diffmin(t0)} minutes'))