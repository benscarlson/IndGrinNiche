#!/usr/bin/env Rscript --vanilla

# ==== Breezy setup ====

'
Saves the median location of movement data for each niche set
Used to calculate dist2nest when an individual is not breeding or when nest position is not known
See layers/dist2nest2.r

Usage:
median_location.r <dat> <out> [-t] [--seed=<seed>]
median_location.r (-h | --help)

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

  .wd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .datPF <- file.path(.wd,'data/dat.csv')
  .outPF <- file.path(.wd,'data/median_location.csv')
  
} else {
  library(docopt)
  library(rprojroot)

  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  #.list <- trimws(unlist(strsplit(ag$list,',')))
  .datPF <- ifelse(isAbsolute(ag$dat),ag$dat,file.path(.wd,ag$dat))
  .outPF <- ifelse(isAbsolute(ag$out),ag$out,file.path(.wd,ag$out))
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(.seed)
t0 <- Sys.time()

source(rd('src/startup.r'))

# suppressWarnings(
#   suppressPackageStartupMessages({
#     library(DBI)
#     library(RSQLite)
#   }))

source(rd('src/funs/breezy_funs.r'))

#---- Local parameters ----#
#.dbPF <- file.path(.wd,"data/database.db")
.outliersPF <- file.path(.wd,'outliers.csv')

#---- Load control files ----#
nsets <- read_csv(file.path(.wd,'ctfs/niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'ctfs/niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

#---- Initialize database ----#
# db <- dbConnect(RSQLite::SQLite(), .dbPF)
# invisible(assert_that(length(dbListTables(db))>0))
# 
# std <- tbl(db,'study')

#---- Load data ----#
message('Loading data...')
dat0 <- read_csv(.datPF,col_types=cols()) %>%
  inner_join(niches %>% select(niche_set,niche_name),by='niche_name')
outliers <- read_csv(.outliersPF,col_types=cols()) %>%
  mutate(outlier=TRUE)

#====

#---- Perform analysis ----#

# dbExecute(db,'PRAGMA foreign_keys=ON')
# dbBegin(db)

message('Calculating median locations')

datRaw <- dat0 %>%   
  left_join(outliers %>% select(row_index,outlier), by='row_index') %>%
  mutate(outlier=ifelse(is.na(outlier),FALSE,TRUE)) %>%
  filter(!outlier) %>%
  select(-outlier)

invisible(assert_that(nrow(dat0)==nrow(datRaw) + nrow(outliers))) #Should be true

datMed <- datRaw %>%
  # inner_join(
  #   niches %>% select(individual_id,year,niche_name),
  #   by=c('niche_name','individual_id')) %>%
  group_by(niche_name,niche_set) %>% #,individual_id,year
  summarize(lon=median(lon),lat=median(lat)) %>% 
  arrange(niche_set,niche_name) %>%
  select(-niche_set) %>%
  ungroup

#---- Save output ---#
message('Saving output')

dir.create(dirname(.outPF),recursive=TRUE,showWarnings=FALSE)

datMed %>% write_csv(.outPF)

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

# if(.test) {
#   message('Rolling back transaction because this is a test run.')
#   dbRollback(db)
# } else {
#   dbCommit(db)
# }
# 
# dbDisconnect(db)

message(glue('Script complete in {diffmin(t0)} minutes'))