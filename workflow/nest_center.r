#!/usr/bin/env Rscript --vanilla

# This script implements the breezy philosophy: github.com/benscarlson/breezy

# ==== Breezy setup ====

'
Figures out nest/central location for each individual/year. Uses nest if that is available, or uses the median location if that is not available.

Usage:
nestcenter.r <out> [--seed=<seed>] [-t]
nestcenter.r (-h | --help)

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

  .wd <- '~/projects/ms1/analysis/rev2/null3_poc3'
  .seed <- NULL
  .rollback <- TRUE
  .test <- TRUE
  rd <- here::here
  
  .outPF <- file.path(.wd,'nest_center.csv')
  
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
  
  .outPF <- ifelse(isAbsolute(ag$out),ag$out,file.path(.wd,ag$out))
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

#Source all files in the auto load funs directory
list.files(rd('src/funs/auto'),full.names=TRUE) %>%
  walk(source)

#---- Local parameters ----#
.dbPF <- '~/projects/whitestork/src/db/db.sqlite'
.medlocPF <- '~/projects/ms1/analysis/huj_eobs/data/median_location.csv'

#---- Load control files ----#
nsets <- read_csv(file.path(.wd,'ctfs/niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'ctfs/niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

#---- Initialize database ----#
invisible(assert_that(file.exists(.dbPF)))
db <- dbConnect(RSQLite::SQLite(), .dbPF)
invisible(assert_that(length(dbListTables(db))>0))

nestTb <- tbl(db,'stork_breeding_data')

#---- Load data ----#
message('Loading data...')

medlocs <- read_csv(.medlocPF,col_types=cols()) %>%
  rename(med_lon=lon,med_lat=lat)
#====

#---- Perform analysis ----#

#Assumes that if nest_lon is na then both nest_lon & nest_lat are na
message('Assigning nest/median locations')
nests <- niches %>%
  left_join(
    nestTb %>% select(individual_id,year,nest_lon,nest_lat) %>% as_tibble,
    by=c('individual_id','year')
  ) %>% 
  left_join(medlocs,by='niche_name') %>% 
  mutate(
    type=ifelse(is.na(nest_lon),'median','nest'),
    nest_lon=ifelse(is.na(nest_lon),med_lon,nest_lon),
    nest_lat=ifelse(is.na(nest_lat),med_lat,nest_lat)) %>%
  select(niche_name,type,nest_lon,nest_lat)

#---- Save output ---#
message('Saving')
dir.create(dirname(.outPF),recursive=TRUE,showWarnings=FALSE)

nests %>% write_csv(.outPF)


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