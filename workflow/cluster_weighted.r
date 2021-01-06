#!/usr/bin/env Rscript --vanilla

# ==== Breezy setup ====

'
Calculates nicheset level weighted clustering index
sesid - can be a comma-seperated list of session ids

Usage:
cluster_weighted.r <sesid> [-t] [--seed=<seed>]
cluster_weighted.r (-h | --help)

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
    library(tnet)
  }))

source(rd('src/funs/breezy_funs.r'))

#---- Local parameters ----#
.dbPF <- '~/projects/whitestork/results/stpp_models/huj_eobs/data/database.db'
.minOverlap <- 0.05
.measure <- 'am' #c("am", "gm", "ma", "mi")

#---- Initialize database ----#
db <- dbConnect(RSQLite::SQLite(), .dbPF)
invisible(assert_that(length(dbListTables(db))>0))

nichetb <- tbl(db,'niche')
ntb <- tbl(db,'niche_stats')
nsettb <- tbl(db,'niche_set_stats')
pwtb <- tbl(db,'pairwise')


#====

message(glue('Calculating nicheset weighted clustering index for session ids: {glue_collapse(.sesid,sep=", ")}'))

#---- Perform analysis ----#
#join twice to get volumns for both niches

#Need to make an id for each niche in order for clustering_w() to work
niches <- nichetb %>% as_tibble %>% mutate(niche_id=row_number())

dat <- pwtb %>%
  select(pairwise_id,ses_id,rep,niche_name1,niche_name2,hv_intr_vol,hv_union_vol) %>%
  filter(ses_id %in% .sesid) %>%
  as_tibble %>%
  inner_join(
    niches %>% select(niche_name,niche_id1=niche_id,niche_set),
    by=c('niche_name1'='niche_name')) %>%
  inner_join(
    niches %>% select(niche_name,niche_id2=niche_id), #already got niche_set with first join
    by=c('niche_name2'='niche_name')) %>%
  mutate(jrd=hv_intr_vol/hv_union_vol) %>%
  filter(jrd >= .minOverlap) 

message('Calculating nicheset clustering...')

cldat <- dat %>%
  select(ses_id,rep,niche_set,i=niche_id1,j=niche_id2,w=jrd) %>%
  nest(data1=-c(ses_id,rep,niche_set)) %>%
  mutate(data2=map(data1,~{
    rbind(.,data.frame(i=.$j,j=.$i,w=.$w)) #have to make symetric links for cluster_w to work
  })) %>%
  mutate(clust_w=map_dbl(data2,~{
    clustering_w(as.matrix(.),measure=.measure)
  })) %>%
  select(ses_id,rep,niche_set,clust_w) %>%
  inner_join( #Join to get the niche_set_stats id
    nsettb %>% as_tibble %>% select(niche_set_stats_id,ses_id,rep,niche_set),
    by=c('ses_id','rep','niche_set')) %>%
  select(niche_set_stats_id,everything())

#Sanity checks
nrowset <- nsettb %>% filter(ses_id %in% .sesid) %>% as_tibble %>% nrow
invisible(assert_that(nrowset == nrow(cldat)))
invisible(assert_that(identical(sort(unique(cldat$ses_id)),sort(.sesid))))
invisible(assert_that(nrowset == length(cldat$niche_set_stats_id)))
invisible(assert_that(length(unique(cldat$niche_set_stats_id)) == length(cldat$niche_set_stats_id)))

invisible(dbExecute(db,'PRAGMA foreign_keys=ON'))
dbBegin(db)

sql <- glue_sql(
'update niche_set_stats 
set clust_w = $clust_w
where niche_set_stats_id = $niche_set_stats_id')

message(glue('Inserting {nrow(cldat)} rows'))

rs <- dbSendStatement(db, sql) #parameter names should match column names
dbBind(rs,params=cldat %>% select(niche_set_stats_id,clust_w))
rows <- dbGetRowsAffected(rs)
dbClearResult(rs)

if(nrow(cldat) != rows) {
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