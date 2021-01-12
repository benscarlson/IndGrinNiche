#!/usr/bin/env Rscript

#TODO: why am I doing trim_dates as a seperate process? I should just do it in obs.r

spsm <- suppressPackageStartupMessages

spsm(library(bencmisc))
spsm(library(dplyr))
spsm(library(fs))
spsm(library(glue))
spsm(library(readr))

source('~/projects/rsf/src/scripts/funs.r')

#---- parameters ----#
if(interactive()) {
  .resultsP <- '/Users/benc/projects/whitestork/results/stpp_models/huj_eobs'
  .datInPF <- 'data/obs.csv'
  .datOutPF <- 'data/obs_trim.csv'
} else {
  .resultsP <- getwd()

  args <- commandArgs(trailingOnly=TRUE)
  .datInPF <- args[1]
  .datOutPF <- args[2]
}

pars <- loadParams(.resultsP)

datInPF <- ifelse(is_absolute_path(.datInPF),.datInPF,file.path(.resultsP,.datInPF))
datOutPF <- ifelse(is_absolute_path(.datOutPF),.datOutPF,file.path(.resultsP,.datOutPF))

#---- load data ----#
message(glue('Loading {datInPF}'))
dat0 <- read_csv(datInPF,col_types=cols())
niches <- read_csv(file.path(.resultsP,'niches.csv'),col_types=cols()) #TODO: check the run field?
nicheSets <- read_csv(file.path(.resultsP,'niche_sets.csv'),col_types=cols())

#---- trim the temporal extents of the niches ----
dat <- dat0 %>% 
  left_join(niches %>% select(niche_name,niche_set),by='niche_name') %>%
  left_join(nicheSets,by='niche_set') %>%
  filter(timestamp >= start_date & timestamp < end_date) %>%
  select(row_index,lon,lat,timestamp,niche_name,individual_id)

#---- save the dataset
message(glue('Saving {datOutPF}'))
write_csv(dat, datOutPF)

message('Script complete')