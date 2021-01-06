#!/usr/bin/env Rscript --vanilla

# ==== Breezy setup ====

'
Uses ADKE home ranges to generate background points.
This samples from the set of all dates that have observations in <dat>
Sampling is weighted by the proportion of the number of obs for each day
So, days with no observations will not have background points

Usage:
bg_homerange.r <dat> <out> [-t] [--seed=<seed>]
bg_homerange.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run, will not save output parameters or commit to git
' -> doc

isAbsolute <- function(path) {
  grepl("^(/|[A-Za-z]:|\\\\|~)", path)
}

#---- Input Parameters ----#
if(interactive()) {
  library(here)

  .wd <- '~/projects/ms1/analysis/bg_lbg2015'
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .datPF <- rd('data/derived/obs_anno.csv')
  .outPF <- file.path(.wd,'data/bg_hr_lbg2015.csv')
  
} else {
  library(docopt)
  library(rprojroot)

  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .datPF <- ifelse(isAbsolute(ag$dat),ag$dat,file.path(.wd,ag$dat))
  .outPF <- ifelse(isAbsolute(ag$out),ag$out,file.path(.wd,ag$out))
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(.seed)
t0 <- Sys.time()

source(rd('src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(sf)
  }))

source(rd('src/funs/breezy_funs.r'))

#---- Local parameters ----#
.akdePF <- '~/projects/whitestork/results/stpp_models/huj_eobs/ctmm/akde_contour/contours.shp'

#---- Load control files ----#
nsets <- read_csv(file.path(.wd,'ctfs/niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'ctfs/niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

#---- Initialize database ----#

#---- Load data ----#
message('Loading data...')

dat0 <- read_csv(.datPF,col_types=cols()) %>%
  inner_join(niches %>% select(niche_set,niche_name),by='niche_name')

akde <- st_read(.akdePF) %>%
  filter(est=='est') %>%
  rename(niche_name=n_name) %>%
  inner_join(niches %>% select(niche_name),by='niche_name')

#====

#---- Perform analysis ----#

num <- dat0 %>% 
  group_by(niche_name) %>% 
  summarize(num=n())

message('Generating random background points...')
t1 <- Sys.time()

bg <- akde %>%
  st_transform(3035) %>%
  inner_join(num,by='niche_name') %>%
  mutate(pts=map2(geometry,num,~{
    st_sample(.x,.y)
  })) %>%
  unnest(pts) %>%
  mutate(pts=st_sfc(pts))
  
message(glue('Complete in {diffmin(t1)} minutes'))

#---- clean up ----#

ptsbg <- bg %>%
  st_set_geometry(.$pts) %>% #change the geometry
  select(niche_name) %>%
  st_set_crs(3035) %>%
  st_transform(crs=4326)

#Convert to tibble
datbg <- ptsbg %>%
  st_set_geometry(NULL) %>%
  bind_cols(
    ptsbg %>% st_coordinates %>% as_tibble) %>%
  rename(lon=X,lat=Y)

dates <- dat0 %>% 
  group_by(niche_name,date=as.Date(timestamp)) %>% 
  summarize(num=n()) %>%
  nest(dates=-niche_name)

#Assign timestamps
#This samples from the set of all dates that have observations
# It samples in the proportion of the number of obs for each day
# So, days with no observations will not have background points
datbgt <- datbg %>% 
  nest(pts=c(lon,lat)) %>%
  inner_join(dates,by='niche_name') %>%
  mutate(data2=pmap(list(pts,dates),
    function(pts,dates) {
      #d <- seq(min(dates$date),max(dates$date),by='1 day')
      w <- dates$num/sum(dates$num)
      pts$timestamp <- sample(dates$date,size=sum(dates$num),replace=TRUE,prob=w)
      return(pts)
    })) %>%
  select(niche_name,data2) %>%
  unnest(data2) %>% 
  arrange(niche_name,timestamp)

#---- Save output ---#
message('Saving output...')
dir.create(dirname(.outPF),recursive=TRUE,showWarnings=FALSE)

datbgt %>% write_csv(.outPF)

#---- Finalize script ----#

if(!.test) {
  library(git2r)
  library(uuid)
  
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

message(glue('Script complete in {diffmin(t0)} minutes'))