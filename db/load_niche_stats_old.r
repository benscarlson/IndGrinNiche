#loading results that were saved to csv to the database
# TODO: Make this into a script

library(DBI)
library(RSQLite)

#---- Parameters ----#

.pd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
.hvjob <- '5axes2000pts1'

#---- Paths ----#
fP <- file.path(.pd,'hvs',.hvjob,'niche_vols.csv')
dbP <- file.path(.pd,"data/database.db")

#---- Load data ----#
db <- DBI::dbConnect(RSQLite::SQLite(), dbP)
vols <- read_csv(fP)

vols %>%
  mutate(indiv_vol_id=NA,hv_job_name=.hvjob) %>%
  select(indiv_vol_id,hv_job_name,niche_name,indiv_vol) %>%
  dbAppendTable(db, "indiv_vol", .)
