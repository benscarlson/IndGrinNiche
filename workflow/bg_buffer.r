#!/usr/bin/env Rscript --vanilla

# ==== Breezy setup ====

'
Uses buffer approach to generate backgrounds. 
Splits temporal extent into 14 day slices
Samples 10 * the number of points found in each time slice.

dat: point dataset with dist2nest column. Used to figure out the size of the buffers
out: name of the output file
cdat: file with centroid (nest/median) for each individual/year

Usage:
bg_buffer.r <dat> <out> <cdat> [-t] [--seed=<seed>]
bg_buffer.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run, will not save output parameters or commit to git
' -> doc

#---- Input Parameters ----#
if(interactive()) {
  library(here)

  .wd <- '~/projects/ms1/analysis/rev2/null3_poc3'
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .cdatPF <- file.path(.wd,'data/nest_center.csv')
  .datPF <- rd('data/derived/obs_anno.csv')
  .outPF <- file.path(.wd,'data/bg_buf.csv')
  
} else {
  library(docopt)
  library(rprojroot)

  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  source(rd('src/funs/input_parse.r'))
  
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

source(rd('src/funs/auto/breezy_funs.r'))

#---- Local parameters ----#
.ndays <- 14
.width <- glue('{.ndays} days')
.flatproj <- 3035
.nx <- 10

#---- Load control files ----#
nsets <- read_csv(file.path(.wd,'ctfs/niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'ctfs/niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

#---- Initialize database ----#

#---- Load data ----#
message('Loading data...')
dat0 <- read_csv(.datPF,col_types=cols())

#FOR TESTING ONLY!!!!
# dat00 <- read_csv(.datPF,col_types=cols())
# dat0 <- dat00 %>%
#   inner_join(niches %>% select(niche_set,niche_name),by='niche_name') %>% 
#   group_by(niche_name) %>% sample_n(500) %>% ungroup

nests <- read_csv(.cdatPF,col_types=cols()) 

#====

#---- Perform analysis ----#

#This section uses observed values for dist2nest to derive 95% quantiles for movement over a 14 day period
dat95 <- dat0 %>%
  mutate(start_date=cut(as.Date(timestamp),.width)) %>%
  group_by(niche_name,start_date) %>%
  summarize(d95_m=quantile(dist2nest,.95),num=n()) %>%
  ungroup %>%
  mutate(end_date=as.Date(as.character(start_date))+(.ndays-1)) %>%
  select(niche_name,start_date,end_date,d95_m,num)

#Note: since I used obs_anno.csv, dist2nest calculation has distances for non-breeding 
#   individuals. These are based on distances from the center of their movements.
# 
buf <- dat95 %>%
  inner_join(niches %>% select(niche_name,individual_id,year),by='niche_name') %>%
  inner_join(nests, by='niche_name') %>%
  st_as_sf(coords=c('nest_lon','nest_lat'),crs=4326) %>%
  st_transform(.flatproj) %>% 
  mutate(geometry=st_buffer(geometry,d95_m))


# buf %>%
#   select(niche_name) %>%
#   ggplot() +
#   geom_sf(aes(color=niche_name),fill=NA)

message('Generating random background points. Takes awhile...')
t1 <- Sys.time()

#Taking num*10 random points in each time window. This mimics how the available distribution is created when doing an SSF.
ptsbg <- buf %>%
  mutate(pts=map2(geometry,num*.nx,~{
    st_sample(.x,.y)
  })) %>%
  unnest(pts) %>%
  mutate(pts=st_sfc(pts)) %>% 
  st_set_geometry(.$pts) %>% #change the geometry. Note reference using the full dataframe context
  st_set_crs(.flatproj) %>%
  select(niche_name,start_date,end_date) #remember geometry columns are sticky

message(glue('Complete in {diffmin(t1)} minutes'))

#Sanity test
invisible(assert_that(sum(buf$num)*.nx==nrow(ptsbg)))

#Can't handle many points, just use to test a few buffers
# ggplot(bg) +
#   geom_sf(aes(color=bin),fill=NA) +
#   geom_sf(aes(color=bin,geometry=pts))

#Convert to tibble
datbg <- ptsbg %>%
  st_transform(crs=4326) %>%
  bind_cols(
    st_sf(.) %>% st_coordinates %>% as_tibble) %>%
  st_set_geometry(NULL) %>%
  rename(lon=X,lat=Y)

#randomly assign days to points based on start, end date of bins
datbgt <- datbg %>% 
  nest(data=c(lon,lat)) %>%
  mutate(start_date=as.Date(as.character(start_date))) %>%
  mutate(data2=pmap(list(start_date,end_date,data),
    function(start_date,end_date,data) {
      d <- seq(start_date,end_date,by='1 day')
      data$timestamp <- sample(d,size=nrow(data),replace=TRUE)
      return(data)
    })) %>%
  select(-data) %>%
  unnest(data2) %>% 
  arrange(niche_name,timestamp)

#---- Save output ---#
message(glue('Saving to {.outPF}'))
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