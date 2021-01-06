#!/usr/bin/env Rscript

spsm <- suppressPackageStartupMessages

spsm(library(amt))
spsm(library(bencmisc))
spsm(library(fs))
spsm(library(glue))
spsm(library(sf))
spsm(library(tidyverse))

filter <- dplyr::filter
select <- dplyr::select

source('~/projects/rsf/src/scripts/funs.r')

#---- parameters ----#
if(interactive()) {
  .resultsP <- '/Users/benc/projects/whitestork/results/stpp_models/huj_eobs'
  .datInPF <- 'data/obs_trim.csv'
  .datOutPF <- 'data/obsbg.csv'
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

#---- generate background points ----#
#TODO: can't I just use spTransDf?
#TODO: select flat proj from parameters file
dat1 <- dat0 %>% 
  st_as_sf(coords=c("lon", "lat"), crs=4326) %>% 
  st_transform(3035) %>%
  sf2df

trks <- dat1 %>% 
  nest(-c(individual_id,niche_name)) %>%
  mutate(trk = map(data, function(d) {
    make_track(tbl=d,.x=x,.y=y, .t=timestamp, crs=sp::CRS('+init=epsg:3035'))
  }))

#TODO: put rate and tolerance into params file?
ssf <- trks %>%
  mutate(rs = map(trk, function(x) {
    x <- x %>% 
      track_resample(rate=minutes(2),tolerance=hours(4)) %>%
      filter_min_n_burst %>% #minimum number of samples per bursts. default is min_n=3
      steps_by_burst %>% #this must make steps based on bursts? i.e. will not make steps between bursts?
      random_steps #number of random points. default is 10 
  })) %>% 
  select(individual_id,niche_name,rs) %>%
  unnest %>%
  mutate(stratum=paste(niche_name,step_id_,sep='_')) %>% #per Stephanie, stratum should be unique across individuals
  select(individual_id, niche_name, obs=case_, stratum, x=x2_,y=y2_,timestamp=t2_)

#---- save the dataset
message(glue('Saving {datOutPF}'))

ssf %>% spTransDf(fromCRS=CRS('+init=epsg:3035')) %>% 
  select(lon=x,lat=y,timestamp,niche_name,individual_id,obs,stratum) %>%
  write_csv(datOutPF)

message('Script complete')