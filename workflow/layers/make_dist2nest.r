#---- commandline arguments ----#
args <- commandArgs(trailingOnly=TRUE)
cellRes <- as.integer(args[1])
if(is.na(cellRes)) stop('Resolution required')

spsm <- suppressPackageStartupMessages

spsm(library(dplyr))
spsm(library(glue))
spsm(library(raster))
spsm(library(readr))
spsm(library(sp))

filter <- dplyr::filter
select <- dplyr::select

source('~/projects/rsf/src/scripts/funs.r')

if(interactive()) {
  .resultsP <- '/Users/benc/projects/whitestork/results/stpp_models/dromling_sum13'
  cellRes <- 30
} else {
  .resultsP <- getwd()
}

message(glue('.resultsP is {.resultsP}'))
message(glue('cellRes is {cellRes}'))

pars <- loadParams(.resultsP)

#---- load data ----#

message('Loading data...')
niches <- read_csv(file.path(.resultsP,'niches.csv'),col_types=cols())
dat0 <- read_csv(file.path(.resultsP,'data/obsbg.csv'),col_types=cols()) #%>% 
  #left_join(select(niches,individual_id,short_name),by='individual_id')
fr <- read_csv('~/projects/whitestork/data/derived/shay/franzmitters.csv',col_types=cols())

#set up nest location and convert to UTM EU

for(i in 1:nrow(niches)) { #
  #i <- 1
  niche <- niches[i,]
  
  message(sprintf('Creating dist2nest raster for %s...',niche$niche_name))
  
  pts <- dat0 %>% filter(niche_name==niche$niche_name)
  coordinates(pts) <- ~lon+lat
  proj4string(pts) <- '+proj=longlat +datum=WGS84'
  pts <- spTransform(pts,CRS(pars$flatProj))
  
  #get extent of study area for reference raster
  refRast <- raster(extent(pts),crs=CRS(pars$flatProj))
  res(refRast) <- cellRes
  
  #get nest location
  spNest <- fr %>% filter(individual_id==niche$individual_id & breeding_year==niche$year) %>%
    select(lon=nest_lon,lat=nest_lat)
  
  #spNest <- data.frame(lon=ent$nest_lon,lat=ent$nest_lat)
  coordinates(spNest) <- ~lon+lat
  proj4string(spNest) <- '+proj=longlat +datum=WGS84'
  spNest <- spTransform(spNest,CRS(pars$flatProj))
  
  #make distance to nest raster
  dist2nest <- distanceFromPoints(refRast,spNest)
  names(dist2nest) <- glue('dist2nest_{cellRes}m')
  
  #plot(dist2nest); points(spNest)
  
  dir.create(file.path(.resultsP,'layers',niche$niche_name),recursive=TRUE,showWarnings=FALSE)
  
  writeRaster(dist2nest, format="raster", overwrite=TRUE,  
              file.path(.resultsP,'layers',niche$niche_name,glue('dist2nest_{cellRes}m.grd')))
  
  #also make log.dist2nest
  log.dist2nest <- log(dist2nest)
  names(log.dist2nest) <- glue('log.dist2nest_{cellRes}m')
  
  writeRaster(log.dist2nest, format="raster", overwrite=TRUE,  
              file.path(.resultsP,'layers',niche$niche_name,glue('log_dist2nest_{cellRes}m.grd')))
  
}

message('Script complete.')