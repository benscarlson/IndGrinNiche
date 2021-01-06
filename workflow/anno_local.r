args <- commandArgs(trailingOnly=TRUE)
filePN <- args[1]
if(is.na(filePN)) stop('path and file name of script to be annotated is required.')

#TODO: pass in name of output file

library(dplyr)
library(glue)
library(raster)
library(readr)
library(sp)

source('~/projects/rsf/src/scripts/funs.r')

#---- parameters ----
if(interactive()) {
  .resultsP <- '/Users/benc/projects/whitestork/results/stpp_models/dromling_sum13'
  filePN <- 'data/obsbg_anno.csv'
} else {
  .resultsP <- getwd()
}

message(glue('.resultsP is {.resultsP}'))
message(glue('filePN is {filePN}'))

#---- load data ----
niches <- read_csv(file.path(.resultsP,'niches.csv'),col_types=cols())
envs <- read_csv(file.path(.resultsP,'envs.csv'),col_types=cols()) %>% 
  filter(source=='local') #%>% filter(as.logical(run))

#if adding, make a copy in case there are errors
message('Making backup copy...')
dir.create(file.path(.resultsP,dirname(filePN),'sav'),recursive=TRUE,showWarnings=FALSE)
file.copy(overwrite=TRUE, 
  file.path(.resultsP,filePN),
  file.path(.resultsP,dirname(filePN),'sav',basename(filePN)))

#TODO: redo code that makes backup copy. if user doesn't give an output file name
# this means add to input file. in this case make a backup copy
# don't need to do this if user supplies a different output name
datAll <- read_csv(file.path(.resultsP,filePN),col_types=cols())

#need to loop through each niche, because of layers like dist2nest, which are specific to the niche

first <- TRUE

for(i in 1:nrow(niches)) {
  #i <- 1
  niche <- niches[i,]
  message(sprintf('Extracting values for %s...',niche$niche_name))
  
  datInd <- datAll %>% filter(niche_name==niche$niche_name)
  
  ptsWRS <- datInd
  coordinates(ptsWRS) <- ~lon+lat
  proj4string(ptsWRS) <- CRS("+proj=longlat +datum=WGS84")
  
  for(j in 1:nrow(envs)) {
    #j <- 1
    env <- envs[j,]
    
    message(sprintf('%s...',env$label))
    message(glue(env$layer))
    envr <- raster(glue(env$layer)) #if layer is individual specific, sub in name
    
    if(proj4string(envr)==proj4string(ptsWRS)) { #note will transform if proj4 is a vairation wgs84
      ptsExtract <- ptsWRS
    } else {
      message('transforming point coordinates...')
      ptsExtract <- spTransform(ptsWRS, crs(envr))    
    }
    message('extracting values...')
    datInd[env$label] <- raster::extract(envr,ptsExtract)
  }

  message('Writing results...')
  write_csv(datInd, file.path(.resultsP,'data/obsbg_anno.csv'),append=!first)
  
  first <- FALSE
}

message('Checking results...')
annoSav <- read_csv(file.path(.resultsP,'data/sav/obsbg_anno.csv'), col_types=cols())
anno <- read_csv(file.path(.resultsP,'data/obsbg_anno.csv'),col_types=cols())
message('Number of rows should match.')
nrow(annoSav); nrow(anno) #should be the same
message('Difference in number of columns should equal number of annotated variables')
ncol(annoSav); ncol(anno) #annoSav should have more columns for each annotated var