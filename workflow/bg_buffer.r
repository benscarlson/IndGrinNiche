#!/usr/bin/env Rscript --vanilla

# ==== Breezy setup ====

'
Uses buffer approach to generate backgrounds

Usage:
bg_buffer.r <dat> <out> [-t] [--seed=<seed>]
bg_buffer.r (-h | --help)

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

  .wd <- '~/projects/ms1/analysis/bsnm'
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .datPF <- rd('data/derived/obs_anno.csv')
  .outPF <- file.path(.wd,'data/full_bg_buf.csv')
  
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
    library(DBI)
    library(RSQLite)
    library(sf)
  }))

source(rd('src/funs/breezy_funs.r'))

#---- Local parameters ----#
.dbPF <- '~/projects/whitestork/src/db/db.sqlite'
.ndays <- 14
.width <- glue('{.ndays} days')
.medlocPF <- '~/projects/whitestork/results/stpp_models/huj_eobs/data/median_location.csv'

#---- Load control files ----#
nsets <- read_csv(file.path(.wd,'ctfs/niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'ctfs/niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

#---- Initialize database ----#
db <- dbConnect(RSQLite::SQLite(), .dbPF)
invisible(assert_that(length(dbListTables(db))>0))

nestTb <- tbl(db,'stork_breeding_data')

#---- Load data ----#
dat0 <- read_csv(.datPF,col_types=cols()) %>%
  inner_join(niches %>% select(niche_set,niche_name),by='niche_name')
medlocs <- read_csv(.medlocPF,col_types=cols()) 

#====

#---- Perform analysis ----#

#Need to get central locations for individuals that don't have nest locations (usually non-breeding)
nests <- niches %>%
  left_join(
    nestTb %>% select(individual_id,year,nest_lon,nest_lat) %>% as_tibble,
    by=c('individual_id','year')
  ) %>% 
  left_join(medlocs,by='niche_name') %>% 
  mutate(
    nest_lon=ifelse(is.na(nest_lon),lon,nest_lon),
    nest_lat=ifelse(is.na(nest_lat),lat,nest_lat)) %>%
  select(niche_name,nest_lon,nest_lat)
  
dat95 <- dat0 %>%
  mutate(start_date=cut(as.Date(timestamp),.width)) %>%
  group_by(niche_name,start_date) %>%
  summarize(d95_m=quantile(dist2nest,.95),num=n()) %>%
  ungroup %>%
  mutate(end_date=as.Date(as.character(start_date))+(.ndays-1)) %>%
  select(niche_name,start_date,end_date,d95_m,num)

#Note: since I used obs_anno.csv, dist2nest calculation has distances for non-breeding 
#   individuals. These are based on distances from the center of their movements.
buf <- dat95 %>%
  inner_join(niches %>% select(niche_name,individual_id,year),by='niche_name') %>%
  inner_join(nests, by='niche_name') %>%
  st_as_sf(coords=c('nest_lon','nest_lat'),crs=4326) %>%
  st_transform(3035) %>% 
  mutate(geometry=st_buffer(geometry,d95_m))


# buf %>%
#   select(niche_name) %>%
#   ggplot() +
#   geom_sf(aes(color=niche_name),fill=NA)

message('Generating random background points...')
t1 <- Sys.time()

bg <- buf %>%
  mutate(pts=map2(geometry,num,~{
    st_sample(.x,.y)
  })) %>%
  unnest(pts) %>%
  mutate(pts=st_sfc(pts))

message(glue('Complete in {diffmin(t1)} minutes'))

#Sanity test
invisible(assert_that(sum(buf$num)==nrow(bg)))

#Can't handle many points, just use to test a few buffers
# ggplot(bg) +
#   geom_sf(aes(color=bin),fill=NA) +
#   geom_sf(aes(color=bin,geometry=pts))

#Clean up
ptsbg <- bg %>% 
  st_set_geometry(bg$pts) %>% #change the geometry
  st_set_crs(3035) %>%
  select(niche_name,start_date,end_date) %>% #remember geometry columns are sticky
  st_transform(crs=4326)

#Convert to tibble
datbg <- ptsbg %>%
  st_set_geometry(NULL) %>%
  bind_cols(
    ptsbg %>% st_coordinates %>% as_tibble) %>%
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

#If transaction is not active, then this code will fail
# currently no way to test for active transaction: https://github.com/r-dbi/DBI/issues/316
# use try() but should come up with a way that does not produce an error message
# if(.test) {
#   message('Rolling back transaction because this is a test run.')
#   try(dbRollback(db))
# } else {
#   try(dbCommit(db))
# }

dbDisconnect(db)

message(glue('Script complete in {diffmin(t0)} minutes'))