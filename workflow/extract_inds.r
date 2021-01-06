#!/usr/bin/env Rscript

spsm <- suppressPackageStartupMessages

spsm(library(DBI))
spsm(library(dplyr))
spsm(library(glue))
spsm(library(lubridate))
spsm(library(readr))
spsm(library(RSQLite))

#---- parameters ----
if(interactive()) {
  .resultsP <- '/Users/benc/projects/whitestork/results/stpp_models/loburg_jun14'
} else {
  .resultsP <- getwd()
}

message(glue('.resultsP is {.resultsP}'))

source('~/projects/rsf/src/scripts/funs.r')
#---- load data ----
niches <- read_csv(file.path(.resultsP,'niches.csv'),col_types=cols())
message('Loading data, takes awhile...')
dat0 <- read_csv('~/projects/whitestork/data/derived/shay/storks_4ben.csv',col_types=cols())
phen2_0 <- read_csv('~/projects/whitestork/src/db/phen2.csv', col_types=cols())

#---- Loop over niches and extract data ----

first <- TRUE
totRows <- 0
for(i in 1:nrow(niches)) { #
  #i<-1
  niche <- niches[i,]
  
  phen2 <- phen2_0 %>% 
    filter(individual_id==niche$individual_id & year(start_date)==niche$year & phen=='summer')
  
  stopifnot(nrow(phen2)==1)
  
  #pick the start date of the niche. This is set in niches.csv, but if migration occurs after
  #the date set in niches then niche should start later.
  #start date in phen2 is the last day that contains migratory movement
  # so, start should occur after this day. This is why we add 1 day to the phen2$start_date
  #end date in phen2 is the first day that contains migratory movement.
  # so, end should occur before this day. No need to add.
  
  start <- max(phen2$start_date + days(1),niche$start_date) #take the latest start date between phen and niche
  end <- min(phen2$end_date,niche$end_date) #take the earliest end date between phen and niche
  
  #assign niche_name here
  dat1 <- dat0 %>% dplyr::filter(
    individual_id==niche$individual_id & 
      timestamp >= start & 
      timestamp < end) %>%
    mutate(individual_id=niche$individual_id,niche_name=niche$niche_name) %>%
    select(row_index,lon,lat,timestamp,niche_name,individual_id,behav,behav2)
  
  if(first) {
    dir.create(file.path(.resultsP,'data'),recursive=TRUE,showWarnings=FALSE)
  }
  
  totRows <- totRows + nrow(dat1)
  message(glue('Writing {nrow(dat1)} rows for {niche$niche_name}.'))
  write_csv(dat1, file.path(.resultsP,'data','dat.csv'), append=!first)
  
  rm(niche); rm(phen2); rm(dat1)
  
  first <- FALSE
}

message(glue('Wrote a total of {totRows} rows.'))

message('Check data...')
datCheck <- read_csv(file.path(.resultsP,'data','dat.csv'),col_types=cols())
nrow(datCheck)
datCheck %>% group_by(individual_id) %>% summarize(num=n())

logOutput('dat.csv','extract_inds.r')
