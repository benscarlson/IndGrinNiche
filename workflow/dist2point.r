#!/usr/bin/env Rscript --vanilla
# chmod 744 script_template.r #Use to make executable

# This script implements the breezy philosophy: github.com/benscarlson/breezy

# ==== Breezy setup ====

'
Annotate point dataset with linear distance to a single point.
Can pass in one point per group.
Right now this is used for dist2nest but can make this more general in the future

dat: point dataset
out: name of the output file
cdat: file with centroid (nest/median) for each individual/year

Usage:
dist2point.r <dat> <out> <cdat> [--colname=<colname>] [--seed=<seed>] [-t]
dist2point.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-c --colname=<colname> Name of annotated column. Defaults to "dist"
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run, will not save output parameters or commit to git
' -> doc

#---- Input Parameters ----#
if(interactive()) {
  library(here)

  .wd <- '~/projects/ms1/analysis/rev2/null3_bg_buf'
  .seed <- NULL
  .test <- TRUE
  rd <- here::here
  
  .colname <- 'dist2nest'
  .cdatPF <- file.path(.wd,'data/nest_center.csv')
  .datPF <- file.path(.wd,'data/bg_buf.csv')
  .outPF <- file.path(.wd,'data/bg_dist.csv')
  
} else {
  library(docopt)
  library(rprojroot)

  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .rollback <- as.logical(ag$rollback)
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  source(rd('src/funs/input_parse.r'))
  
  .colname <- ifelse(is.null(ag$colname),'dist',ag$colname)
  .cdatPF <- makePath(ag$cdat)
  .datPF <- makePath(ag$dat)
  .outPF <- makePath(ag$out)
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

#Source all files in the auto load funs directory
list.files(rd('src/funs/auto'),full.names=TRUE) %>%
  walk(source)

#---- Local parameters ----#
.flatproj <- 3035

#---- Load control files ----#
nsets <- read_csv(file.path(.wd,'ctfs/niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'ctfs/niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

#---- Load data ----#
message('Loading data...')
dat0 <- read_csv(.datPF,col_types=cols()) %>%
  inner_join(niches %>% select(niche_name),by='niche_name') %>%
  mutate(row_id=row_number())

nests <- read_csv(.cdatPF,col_types=cols()) 

#====

#---- Perform analysis ----#

# This converts both nest locations and animal locations to flat projection
# Then calculates euclidean distance between nests and locations

#converts coordinates to planar and calc euclidean dist, which is a lot faster

nestxy <- nests %>%
  st_as_sf(coords=c('nest_lon','nest_lat'),crs=4326) %>%
  select(niche_name) %>%
  st_transform(.flatproj) %>%
  #sfc_as_cols(names=c('nest_x','nest_y')) %>% next two lines do the same thing
  bind_cols(
    st_sf(.) %>% st_coordinates %>% as_tibble) %>%
  st_set_geometry(NULL) %>%
  rename(nest_x=X,nest_y=Y)

datxy <- dat0 %>%
  st_as_sf(coords=c('lon','lat'),crs=4326) %>%
  st_transform(crs=.flatproj) %>%
  bind_cols(
    st_sf(.) %>% st_coordinates %>% as_tibble) %>%
  st_set_geometry(NULL) %>%
  rename(x=X,y=Y)

datDists <- datxy %>%
  left_join(nestxy, by='niche_name') %>%
  mutate(!!.colname:=sqrt((x-nest_x)^2 + (y-nest_y)^2)) %>% #calculate euclidean dist using x,y values
  select(row_id,!!.colname)

#---- Save output ---#
message(glue('Saving to {.outPF}'))

dir.create(dirname(.outPF),recursive=TRUE,showWarnings=FALSE)

#Join to row_id so that I don't need to turn datxy back into sf object and then convert coordinates back to 4326
dat0 %>%
  left_join(datDists,by='row_id') %>%
  select(-row_id) %>%
  write_csv(.outPF)

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