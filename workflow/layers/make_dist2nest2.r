# This script adds dist2nest to a dataframe of tracks
# Pass in a dataframe. Should at have niche_name, lon, lat.
# Add an ephemeral row_id to the df
# Use niches.csv to join to nest data
# Calculate dist2nest for each row, return df with row_id, dist2nest
# Join this df back to original df and return/save.
# TODO: need to make this into a script that can be run from command line
# TODO: make calculation of nest or center of niche a seperate process
#   1) I have nest location from shay
#   2) No nest location because animal did not breed
#   3) I don't have nest location from later years, and I don't know if animal bred or not
#   So, if I have 1, use that. If I know animal did not breed, use center of distribution (#2).
#   If I don't have any information (#3) can use nestr to differentiate breeding/non-breeding to get #1 or #2

#IDEA: Pass in a dataframe of locations, and data frame of nest location
spsm <- suppressPackageStartupMessages

#spsm(library(bencmisc))
spsm(library(DBI))
spsm(library(geosphere))
spsm(library(tidyverse))
spsm(library(RSQLite))
spsm(library(sf))

select <- dplyr::select
filter <- dplyr::filter

source('~/projects/rsf/src/scripts/funs.r')

#---- params ----#
if(interactive()) { #if executing from rstudio
  .pd='~/projects/whitestork/results/stpp_models/huj_eobs'
} else {
  .p <- getwd()
}

.db <- '~/projects/whitestork/src/db/db.sqlite'
.datPF <- file.path(.pd,'data','obsbg_anno.csv')
.outPF <- file.path(.pd,'data','obsbg_anno2.csv')

pars <- loadParams(.pd)

#---- load data ----
niches <- read_csv(file.path(.pd,'niches.csv'),col_type=cols()) %>% 
  filter(as.logical(run))
dat0 <- read_csv(.datPF,col_type=cols())
datRaw0 <- read_csv(file.path(.pd,'data','dat.csv'))
outliers <- read_csv(file.path(.pd,'outliers.csv'),col_types=cols()) %>%
  mutate(outlier=TRUE)
#dat0 <- dat0 %>% select(-dist2nest)

#Note nest data only goes to 2015
#TODO: new dataset from shay should have 2016 data
db <- DBI::dbConnect(RSQLite::SQLite(), .db)
nests <- tbl(db,'stork_breeding_data') %>% as_tibble %>%
  inner_join(
    niches %>% select(individual_id,year,niche_name),
    by=c('individual_id','year'))

#dat will have dist2nest joined to it at the end of the script, using row_id
dat <- dat0 %>% 
  #filter(obs) %>% #TESTING
  mutate(row_id=row_number()) #TODO: make sure row_id doesn't already exist

# Calculate mean location 
# TODO: could calculate this seperatly and store somewhere
# If comparing mean locations to nest locations, then doing behavior==6 is best
# However, we want mean locations where individuals don't have nests
# For this, we should do all behaviors to get center of range

#first, remove outliers
datRaw <- datRaw0 %>%   
  left_join(outliers %>% select(row_index,outlier), by='row_index') %>%
  mutate(outlier=ifelse(is.na(outlier),FALSE,TRUE)) %>%
  filter(!outlier) %>%
  select(-outlier)

nrow(datRaw0)==nrow(datRaw) + nrow(outliers) #Should be true

#meanlocs data goes to 2016
meanlocs <- datRaw %>%
  #filter(behav %in% 6) %>% #5 standing, 6 is sitting.
  inner_join(
    niches %>% select(individual_id,year,niche_name),
    by=c('niche_name','individual_id')) %>%
  group_by(niche_name,individual_id,year) %>%
  summarize(mean_lon=mean(lon),mean_lat=mean(lat)) %>% ungroup

# #This code compares accuracy of mean location to nest location
# #Not something we really care about if we are interested in animals that don't have nests
# x <- nests %>% 
#   select(individual_id,year,nest_lon,nest_lat) %>%
#   inner_join(
#     niches %>% select(individual_id,year,niche_name),
#     by=c('individual_id','year')) %>%
#   left_join(meanlocs,by=c('individual_id','year')) %>%
#   select(niche_name,nest_lon,mean_lon,nest_lat,mean_lat) %>%
#   filter(!(is.na(nest_lon) | is.na(nest_lat)))
# 
# #Very convoluted way to get dist_m to return a single column
# x %>%
#   group_by(niche_name) %>%
#   nest %>%
#   mutate(dist_m=map(data,~{
#     distm(c(.$nest_lon,.$nest_lat),c(.$mean_lon,.$mean_lat),fun=distHaversine)
#   })) %>%
#   unnest
  
# This converts both nest locations and animal locations to flat projection
# Then calculates euclidean distance between nests and locations

#converts coordinates to planar and calc euclidean dist, which is a lot faster
nestxy <- nests %>% 
  select(niche_name,nest_lon,nest_lat) %>%
  full_join(meanlocs,by='niche_name') %>%
  mutate(
    nest_lon=ifelse(is.na(nest_lon),mean_lon,nest_lon),
    nest_lat=ifelse(is.na(nest_lat),mean_lat,nest_lat)) %>%
  st_as_sf(coords=c('nest_lon','nest_lat'),crs=4326) %>%
  select(niche_name) %>%
  st_transform(pars$flatProjCRS) %>%
  sfc_as_cols(names=c('nest_x','nest_y')) %>%
  st_set_geometry(NULL) #this is now a dataframe with nest x and y cols

datxy <- dat %>%
  st_as_sf(coords=c('lon','lat'),crs=4326) %>%
  sfc_as_cols(names=c('lon','lat')) %>%
  st_transform(crs=pars$flatProjCRS) %>%
  sfc_as_cols(names=c('x','y')) %>% #make columns of flat x,y values
  st_set_geometry(NULL)  #%>% #this is now a dataframe with lon, lat cols
  #select(timestamp,lon,lat,niche_name,everything()) #TODO: might not have to worry abut htis
  
datDists <- datxy %>%
  left_join(nestxy, by='niche_name') %>%
  mutate(dist2nest=sqrt((x-nest_x)^2 + (y-nest_y)^2)) %>% #calculate euclidean dist using x,y values
  select(row_id,dist2nest)

dat %>%
  left_join(datDists,by='row_id') %>%
  select(-row_id) %>%
  write_csv(.outPF)

# dat2 <- read_csv(.outPF)
# nrow(dat)==nrow(dat2) #should be true
# ncol(dat %>% select(-row_id))+1==ncol(dat2) #should be true

#---- old code ----#

# #Trying to use aeqd projection. Just decided to go with European flat projection instead
# 
# #This calculates dist2nest using actual distance, instead of creating raster then sampling
# # Since I'm only interested in distance from a single location, it makes sense to use
# # the aeqd projection, which preserves distance
# #https://proj4.org/operations/projections/aeqd.html
# 
# #set lon_0 and lat_0 to nest coords
# '+proj=aeqd +ellps=WGS84 +lon_0= +lat_0='
# filter(dist2nest > pars$nestForageDist_m | is.na(dist2nest)) %>% #if non breeding, dist is NA
# 
# dat2 <- niches %>%
#   left_join(
#     breed0 %>% select(individual_id,year,nest_lon,nest_lat),
#     by=c('individual_id','year')) %>%
#   select(niche_name,nest_lon,nest_lat) %>% 
#   inner_join(dat,by='niche_name') %>%
#   group_by(niche_name) %>%
#   nest
# 
# df <- dat2$data[[1]]
# nest_lon=unique(df$nest_lon)
# nest_lat=unique(df$nest_lat)
# p4s <- glue('+proj=aeqd +ellps=WGS84 +lon_0={unique(df$nest_lon)} +lat_0={unique(df$nest_lat)}')
# pts <- st_as_sf(x=df %>% select(-nest_lon,-nest_lat), coords=c("lon", "lat"), crs=4326)
# st_transform(pts,p4s)