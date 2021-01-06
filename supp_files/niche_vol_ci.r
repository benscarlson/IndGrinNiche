#!/usr/bin/env Rscript --vanilla

# ==== Breezy setup ====

'
Generates CSV of confidence intervals for niche volume, by niche

Usage:
niche_vol_ci.r <out> [-t] [--seed=<seed>]
niche_vol_ci.r (-h | --help)

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
  
  #.datPF <- file.path(.wd,'data/dat.csv')
  .outPF <- file.path(.wd,'supp_files/S1.csv')
  
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
  #.datPF <- ifelse(isAbsolute(ag$dat),ag$dat,file.path(.wd,ag$dat))
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

source(rd('src/funs/breezy_funs.r'))
source(rd('src/funs/themes.r'))
theme_set(theme_eda)

#---- Local parameters ----#
.dbPF <- file.path(.wd,"data/database.db")
.db2PF <- file.path('~/projects/whitestork/src/db/db.sqlite')
.sesid <- 'full_ci'

#---- Load control files ----#
nsets <- read_csv(file.path(.wd,'ctfs/niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'ctfs/niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

#---- Initialize database ----#
db <- dbConnect(RSQLite::SQLite(), .dbPF)
invisible(assert_that(length(dbListTables(db))>0))

nstat <- tbl(db,'niche_stats') %>% 
  filter(ses_id==.sesid) %>% 
  select(niche_name,niche_vol) %>% 
  as_tibble

db2 <- dbConnect(RSQLite::SQLite(), .db2PF)
invisible(assert_that(length(dbListTables(db2))>0))

ind <- tbl(db2,'individual') %>% as_tibble

#---- Load data ----#
# message('Loading data...')
# dat0 <- read_csv(.datPF,col_types=cols()) %>%
#   inner_join(niches %>% select(niche_set,niche_name),by='niche_name')

#====

#---- Perform analysis ----#

niches <- niches %>%
  inner_join(ind %>% select(individual_id,short_name),by='individual_id')

dat <- nstat %>%
  group_by(niche_name) %>%
  summarize(
    mean=mean(niche_vol),
    ci_low=quantile(niche_vol,0.025),
    ci_high=quantile(niche_vol,0.975)) %>%
  inner_join(
    niches %>% select(niche_name,population,year,short_name),
    by='niche_name') %>%
  select(animal_name=short_name,population,year,mean,ci_low,ci_high) %>%
  arrange(population,year,animal_name)

#---- Save output ---#
message('Saving output...')
dir.create(dirname(.outPF),recursive=TRUE,showWarnings=FALSE)

dat %>% write_csv(.outPF)
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
dbDisconnect(db2)

message(glue('Script complete in {diffmin(t0)} minutes'))