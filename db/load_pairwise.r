#!/usr/bin/env Rscript

set.seed(5326)

'
Load pairwise set operations into database

Usage:
hv_pairwise <hvjob>
hv_pairwise (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
' -> doc

source('~/projects/rsf/src/startup.r')

spsm(library(docopt))
spsm(library(DBI))
spsm(library(RSQLite))

#---- Parameters ----#

if(interactive()) {
  .pd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
  .hvjob <- '5axes2000pts1'
} else {
  ag <- docopt(doc, version = '0.1\n')
  .pd <- getwd()
  .hvjob <- ag$hvjob
}

.dbP <- file.path(.pd,"data/database.db")
.pwPF <- file.path(.pd,'hvs',.hvjob,'hv_pairwise.csv')

#---- Load data ----#

db <- DBI::dbConnect(RSQLite::SQLite(), .dbP)
dat <- read_csv(.pwPF)

#---- Insert data into database ----#
dat %>%
  mutate(pairwise_id=NA) %>%
  select(pairwise_id,everything()) %>%
  dbAppendTable(db, "pairwise", .)

message('Script complete')