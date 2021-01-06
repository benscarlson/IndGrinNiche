#!/usr/bin/env Rscript

#TODO: why am I doing trim_dates as a seperate process? I should just do it in this script.

spsm <- suppressPackageStartupMessages

spsm(library(bencmisc))
spsm(library(DBI))
spsm(library(dbplyr))
spsm(library(dplyr))
spsm(library(glue))
#spsm(library(purrrlyr))
spsm(library(readr))
spsm(library(RSQLite))
spsm(library(sf))

source('~/projects/rsf/src/scripts/funs.r')

#---- parameters ----#
if(interactive()) {
  .resultsP <- '/Users/benc/projects/whitestork/results/stpp_models/huj_eobs'
} else {
  .resultsP <- getwd()
}
pars <- loadParams(.resultsP)

#---- load data ----#
#TODO: missing nest data after 2015.
db <- DBI::dbConnect(RSQLite::SQLite(), '~/projects/whitestork/src/db/db.sqlite')
nests <- tbl(db,'stork_breeding_data')
dat0 <- read_csv(file.path(.resultsP,'data','dat.csv'),col_types=cols())

# nests <- read_csv('~/projects/whitestork/data/derived/shay/breeding_data.csv',col_types=cols()) %>%
#   select(individual_id,year,nest_lon,nest_lat)

niches <- read_csv(file.path(.resultsP,'niches.csv'),col_types=cols())

if(file.exists(file.path(.resultsP,'outliers.csv'))) {
  outliers <- read_csv(file.path(.resultsP,'outliers.csv'),col_types=cols()) %>%
    mutate(outlier=TRUE)
} else {
  outliers <- NULL
}

#---------------------
#---- Filter data ----
#---------------------

#---- filter outliers. Skip if there are none.

if(!is.null(outliers)) {
  dat0 <- dat0 %>%
    left_join(outliers %>% select(row_index,outlier), by='row_index') %>%
    mutate(outlier=ifelse(is.na(outlier),FALSE,TRUE)) %>%
    filter(!outlier) %>%
    select(-outlier) #cleanup columns
} else {
  message('Assuming there are no outliers because there is no outlier.csv file.')
}

#---- filter by behavioral class. 

datBehav <-  dat0 %>%
  dplyr::filter(behav %in% pars$behaviors) %>%
  select(-behav,-behav2) #cleanup columns

# "walking, pecking" can also occur near or on nest, but is likely not foraging.
# so, remove any points that are < 100m from nest.

#---- filter by distance to the nest.
#TODO: make some sort of report for this

message(glue('Removing points < {pars$nestForageDist_m} m from nest. Can be slow!'))

#converts coordinates to planar and calc euclidean dist, which is a lot faster
#pars$flatProjEspg <- 3035 #ETRS89 / ETRS-LAEA

ptsNest <- nests %>% 
  filter(!(is.na(nest_lon) | is.na(nest_lat))) %>%
  as_tibble %>%
  st_as_sf(coords=c('nest_lon','nest_lat'),crs=4326) %>%
  select(individual_id, year) %>%
  st_transform(pars$flatProjEspg) %>%
  sfc_as_cols(names=c('nest_x','nest_y')) %>%
  st_set_geometry(NULL) #this is now a dataframe with nest x and y cols

datDist <- datBehav %>%
  st_as_sf(coords=c('lon','lat'),crs=4326) %>%
  sfc_as_cols(names=c('lon','lat')) %>%
  st_transform(crs=pars$flatProjEspg) %>%
  sfc_as_cols(names=c('x','y')) %>% #make columns of flat x,y values
  st_set_geometry(NULL) %>% 
  left_join(niches %>% select(niche_name,year), by='niche_name') %>%
  left_join(ptsNest, by=c('individual_id','year')) %>%
  mutate(dist2nest=sqrt((x-nest_x)^2 + (y-nest_y)^2)) %>% #calculate euclidean dist using x,y values
  filter(dist2nest > pars$nestForageDist_m | is.na(dist2nest)) %>% #if non breeding, dist is NA
  select(-c(x,y,nest_x,nest_y,dist2nest)) #cleanup columns

#Don't think I need obs_id anymore. Remove this! I have row_index now and also not using stpp models anymore.
#Also do I need to set obs=TRUE here? Seems unnecessary... Do this when I create bg points?
# need unique id for each obs. These will be assigned to quadrature points as well. 
# qpts for dummy grids will not have any obs_id
obs <- datDist %>%
  # mutate(
  #   obs_id=row_number(), 
  #   obs=TRUE) %>% 
  arrange(niche_name,timestamp) %>%
  select(row_index,lon,lat,timestamp,niche_name,individual_id) #,obs_id,obs

#---- save the dataset
message('Saving dataset...')
write_csv(obs, file.path(.resultsP,'data','obs.csv'))

#---- check file ----#
#do this using cat or something? Would be much faster...
message('Checking results...')
nrow(obs)
datCheck <- read_csv(file.path(.resultsP,'data','obs.csv'),col_types=cols())
nrow(datCheck)