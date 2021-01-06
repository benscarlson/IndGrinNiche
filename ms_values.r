#!/usr/bin/env Rscript
# chmod 744 script_template.r #Use to make executable

'
Generates all values that are referenced in the ms
Idea is to write these to csv, then somehow link to ms document (using fields?)

Usage:
ms_values.r <hvjob> <out> [--seed=<seed>] [-t]
ms_values.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run
' -> doc

#---- Input Parameters ----#
if(interactive()) {
  library(here)
  
  .wd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
  .script <- 'src/ms_values.r' #Currently executing script
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .outPF <- '~/projects/ms1/docs/ms/v10/ms_values.csv'
  
} else {
  library(docopt)
  library(rprojroot)
  
  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .outPF <- ag$out
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(.seed)
t0 <- Sys.time()

source(rd('src/startup.r'))

spsm(library(DBI))
spsm(library(RSQLite))

spsm(library(sf))

source(rd('src/funs/funs.r'))

#---- Parameters ----#
.dbPF <- file.path(.wd,"data/database.db")
.refdbPF <- '~/projects/whitestork/src/db/db.sqlite'
.year <- 2015
.mod <- 'mod4'
.hvjob='5axes2000pts1'
.rptmod <- 'simple'

#---- Load data ----#

db <- dbConnect(RSQLite::SQLite(), .dbPF)
reptb <- tbl(db,'rpt') %>% filter(hvjob==.hvjob,mod==.mod,rpt_mod==.rptmod)

refdb <- dbConnect(RSQLite::SQLite(), .refdbPF)
breed <- tbl(refdb,'stork_breeding_data')

nsets <- read_csv(file.path(.wd,'niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

dat0 <- read_csv(file.path(.wd,'data/obsbg_anno.csv'),col_types=cols())

#---- Numbers referenced in ms ----#

#-- Abstract --#

#Total indivs used in study
niches %>% distinct(individual_id) %>% nrow 

#Total individual-year combinations
niches %>% nrow

#Broken down by population
# niches %>% distinct(individual_id,population) %>%
#   group_by(population) %>%
#   summarize(num=n())

#-- Methods --#


#----
#---- Results --#
#----

#Number of individuals in Fig2 (loburg 2015)
niches %>% filter(year==.year & population=='loburg') %>% nrow

#Number of individual-year combinations not in loburg 2015
niches %>% filter(!(year==.year & population=='loburg')) %>% nrow

#Average maximum foraging distance from nest

dat0 %>%
  group_by(niche_name) %>%
  summarize(max_dist_indiv=max(dist2nest)) %>%
  summarize(mean_dist_tot=mean(max_dist_indiv),sd_dist_tot=sd(max_dist_indiv))

#Pairwise distance between nests

dists <- breed %>% 
  as_tibble %>% 
  inner_join(niches, by=c('individual_id','year')) %>%
  filter(!(is.na(nest_lon) | is.na(nest_lat))) %>%
  select(niche_name,population,year,nest_lon,nest_lat) %>%
  #filter(population=='dromling' & year %in% c(2013,2014)) %>%
  nest(data=-c(population,year)) %>%
  mutate(dists=map(data,function(d) {
    pts <- st_as_sf(x=d, coords=c('nest_lon','nest_lat'),crs=4326)
    dmtx <- pts %>% st_distance
    
    #dists is a vector of all pair-wise distances
    dists <- dmtx[lower.tri(dmtx)] 
    
    return(dists)
  })) %>%
  select(dists) %>%
  unnest(cols=dists) %>%
  pluck('dists')

mean(dists)
sd(dists)

#mean, range for habitat selection coefficients
reptb %>% filter(trait != 'spec') %>% as_tibble %>%
  summarize(mean=mean(R),min=min(R),max=max(R))

reptb %>% filter(trait == 'spec') %>% select(R, conf_low, conf_high)

#----
#---- Methods --#
#----
obstrm0 <- read_csv(file.path(.wd,'data/obs_trim.csv'))
nrow(obstrm0)
#----
#---- Captions --#
#----

#-- Figure 3 caption --#
niches %>% filter(year==.year) %>% nrow #Num indivs in specified year


#---- Finalize script ----#

if(!.test) {
  spsm(library(git2r))
  spsm(library(uuid))
  
  .runid <- UUIDgenerate()
  .parPF <- file.path(.wd,"run_params.csv")
  
  #Update repo and pull out commit sha
  repo <- repository(rd('src'))
  
  rstat <- status(repo)
  if(length(rstat$staged) + 
     length(rstat$unstaged) + 
     length(rstat$untracked) > 0) {
    add(repo,'.')
    commit(repo, glue('script auto update. runid: {.runid}'))
  }
  
  
  .git_sha <- sha(repository_head(repo))
  
  #Save all parameters to csv for reproducibility
  #TODO: write this to a workflow database instead
  saveParams(.parPF)
}

dbCommit(db)
dbDisconnect(db)

message(glue('Script complete in {diffmin(t0)} minutes'))