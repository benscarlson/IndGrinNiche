#loading results that were saved to csv to the database
# TODO: Make this into a script

spsm <- suppressPackageStartupMessages

spsm(library(DBI))
spsm(library(RSQLite))

.pd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
.hvjob <- '5axes2000pts1'
.nsjob <- 'hvs_niche_set_df3'

#---- Paths ----#
fP <- file.path(.pd,'hvs',.hvjob,.nsjob,'niche_set_vol.csv')
dbP <- file.path(.pd,"data/database.db")

#---- Load data ----#
db <- DBI::dbConnect(RSQLite::SQLite(), dbP)
vols <- read_csv(fP)

vols %>%
  mutate(niche_set_vol_id=NA,hv_job_name=.hvjob) %>%
  select(niche_set_vol_id,hv_job_name,niche_set,niche_set_vol) %>%
  dbAppendTable(db,'niche_set_vol',.)
