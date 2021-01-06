#!/usr/bin/env Rscript

spsm <- suppressPackageStartupMessages

spsm(library(bencmisc))
spsm(library(dplyr))
spsm(library(glue))
spsm(library(readr))
spsm(library(sp))

filter <- dplyr::filter
select <- dplyr::select

source('~/projects/rsf/src/scripts/funs.r')

#---- parameters ----#
if(interactive()) {
  .resultsP <- '/Users/benc/projects/whitestork/results/stpp_models/beuster_summer'
} else {
  .resultsP <- getwd()
}

message(glue('.resultsP is {.resultsP}'))

pars <- loadParams(.resultsP)
niches <- read_csv(file.path(.resultsP,'niches.csv'),col_types=cols())

dat0 <- read_csv(file.path(.resultsP,'data/dat.csv'),col_types=cols()) %>%
  select(row_index,lon,lat,timestamp,niche_name,individual_id)

first <- TRUE
for(i in 1:nrow(niches)) {
  #i <- 1
  niche <- niches[i,]

  mv <- dfToMove(dat0 %>% filter(niche_name==niche$niche_name), animalName=niche$niche_name)
  mv <- spTransform(mv,pars$flatProj)
  
  mv$outlier <- outliers(mv,pars$maxSpeed)
  
  if(any(mv$outlier)) {
    
    outDf <- data.frame(row_index=mv$row_index[mv$outlier])
    outDf$individual_id <- niche$individual_id
    outDf$niche_name <- niche$niche_name
    write_csv(outDf,file.path(.resultsP,'outliers.csv'),append=!first)
    first <- FALSE
    rm(outDf)
  }
  rm(mv)
}

#---- print results ----#
if(file.exists(file.path(.resultsP,'outliers.csv'))) {
  read_csv(file.path(.resultsP,'outliers.csv'),col_types=cols())
  logOutput('outliers.csv','outliers.r')
} else {
  message('No outliers found.') #TODO: should write this to some sort of log
}

message('Script complete.')
#------------------#
#---- OLD CODE ----#
#------------------#
# 
# #4070502, 4084056 #outliers for h5188
# 
# 
# 
# #Some code for identifying outliers
# #Need to run this process iteratively. Identify largest outlier, remove, then repeat
# mv$dists <- c(NA,distance(mv))
# mv$lags <- c(NA,timeLag(mv, units='mins'))
# mv$speed <- c(NA,move::speed(mv))
# mv$angle <- c(NA,angle(mv))
# 
# #dist column for point[i] is the distance from point[i-1] to point[i]
# 
# id<-which(mv$speed==rev(sort(mv$speed))[1])
# rev(sort(mv$dists))[1:10]
# #should see i-1 has a short dist, and i has a large dist
# #The tricky part is max distance will get close to the outlier, but it also could be the 
# # point or point after or before.
# # should see pattern like: small, big, big, small. First 'big' is the outlier.
# # if you see small, big, small, big this is probably not an outlier, unless tag malfunctions in the same location twice.
# as.data.frame(mv[(id-2):(id+2),]) %>% kable()
# 
# mv$outlier <- outliers(mv,20)
# plot(mv[!mv$outlier,], xlim=xlim,ylim=ylim)
# points(mv[mv$outlier,], col='red', pch=19)
# as.data.frame(mv[mv$outlier,]) %>% kable()
# 
# mv2 <- mv[!mv$outlier,]
# mv2$outlier <- outliers(mv2,20)
# plot(mv2[!mv2$outlier,], xlim=xlim,ylim=ylim)
# points(mv2[mv2$outlier,], col='red', pch=19)
# as.data.frame(mv2[mv2$outlier,]) %>% kable()
# 
# points(mv[(id-2):(id+2),], col='red')
# points(mv[na.omit(mv$speed>15),],col='red')
# 
# plot(mv[na.omit(mv$speed<25),],xlim=xlim,ylim=ylim)
# nrow(mv[na.omit(mv$speed>15),])
# plot(mv[c(id-1,id),],xlim=xlim,ylim=ylim)
# text(mv[c(id-1,id),],labels=mv[c(id-1,id),]$row_index)
# plot(mv[c(id,id+1),],xlim=xlim,ylim=ylim)
# text(mv[c(id,id+1),],labels=mv[c(id,id+1),]$row_index)
# plot(mv[c(id+1,id+2),],xlim=xlim,ylim=ylim)
# text(mv[c(id+1,id+2),],labels=mv[c(id+1,id+2),]$row_index)
# 
# (outrow <- mv[id,]$row_index) #might need to set this manually
# outrow <- 4084056
# 
# mv$outlier <- mv$row_index==outrow
# 
# plot(mv,col=ifelse(mv$outlier,'red','blue'),xlim=xlim,ylim=ylim)
# 
# plot(mv[mv$row_index != outrow,],col=ifelse(mv$outlier,'red','blue'),xlim=xlim,ylim=ylim)
# 
# mv <- mv[mv$row_index != outrow,]
# 
# #---- save the dataset again if necessary
# outliers <- c(4070502, 4084056)
# obs <- obs %>% filter(!row_index %in% outliers)
# write_csv(obs, file.path(pars$rootPath,datName,'data/obs.csv'))
# 
# #generate report
# cd /Users/benc/projects/whitestork/src/reports/report_mvtrack
# ~/projects/whitestork/src/scripts/shell/makeMvReport.sh