#!/usr/bin/env Rscript
# chmod 744 script_template.r #Use to make executable

#NOTE: Copied this from ms1_poc to include in the suppment
#   Also see ms1_poc/src/poc/gender_breeding_diffs.r
'
Template

Usage:
hr_niche_size.r <hvjob> <hr> <out> [--seed=<seed>] [-t]
hr_niche_size.r (-h | --help)

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
  .script <- 'src/script_template.r' #Currently executing script
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .hvjob <- '5axes2000pts1'
  .hrP <- file.path(.wd,'ctmm/akde_contour/contours.shp')
  .outPF <- file.path(.wd,'figs/eda/hr_niche_size.pdf')
  
} else {
  library(docopt)
  library(rprojroot)
  
  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .hvjob <- ag$hvjob
  .hrP <- ag$hr
  .outPF <- ag$out
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
  library(units)
}))

source(rd('src/funs/funs.r'))
source(rd('src/funs/themes.r'))
theme_set(theme_eda)

#---- Parameters ----#
.dbPF <- file.path(.wd,"data/database.db")

#---- Load data ----#

db <- dbConnect(RSQLite::SQLite(), .dbPF)
nindtb <- tbl(db,'niche_stats') %>% filter(ses_id==.hvjob)

# refdb <- dbConnect(RSQLite::SQLite(), '~/projects/whitestork/src/db/db.sqlite')
# breed <- tbl(refdb,'stork_breeding_data')

nsets <- read_csv(file.path(.wd,'ctfs/niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'ctfs/niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set') #%>%
  # left_join(breed %>% as_tibble %>% 
  #             select(individual_id,year,eggs,fledglings,breeding),
  #           by=c('individual_id','year'))

# #one-off investigations of 'unknown' breeding status
# niches %>% filter(is.na(breeding) & year != 2016) %>% View
# 
# All individuals with unknown breeding status are from 2016, when no
# breeding information was available for any stroks. The two that are not available
# are both from 2015: HH870 (Dromling), and H4799 (Beuster). We do have breeding
# information for these two storks in both 2013 and 2014. So, it does not seem as if
# these are transient storks.
# #
# #8863811, HH870-2015
# #8863809, H4799-2015
# 
# niches %>% filter(individual_id %in% c(8863811, 8863809))

polys0 <- st_read(.hrP,quiet=TRUE) %>%
  filter(est=='est') %>%
  select(niche_name=n_name) %>% #have to rename b/c shapefile 
inner_join(
  niches %>% select(niche_name),
  by='niche_name')

polys <- polys0 %>% mutate(area_km2=set_units(st_area(.),km^2))

#---- Perform analysis ----#

gdat <- polys %>% 
  st_set_geometry(NULL) %>%
  mutate(area_km2=as.numeric(area_km2)) %>%
  select(niche_name,area_km2) %>%
  inner_join(
    nindtb %>% as_tibble %>% select(niche_name,niche_vol),
    by='niche_name') #%>%
  # mutate(area_km2=as.numeric(area_km2),
  #        breeding=case_when(
  #          breeding==1 ~ 'Breeding',
  #          breeding==0 ~ 'Non-breeding',
  #          is.na(breeding) ~ 'Unknown'
  #        ))

#cor(gdat$area_km2,gdat$indiv_vol) #0.08795231
#cor(log(gdat$area_km2),log(gdat$indiv_vol)) #0.3258229
#m <- lm(log(gdat$niche_vol)~log(gdat$area_km2))
#summary(m) #significant but r^2 is 0.11

p <- ggplot(gdat,aes(x=log(area_km2),y=log(niche_vol))) +
  geom_point() +
  labs(x='log of home range area (km2)',
      y='log of niche volume',
      color=NULL); p 

h=4; w=5
if(fext(.outPF)=='pdf') {
  ggsave(.outPF,plot=p,height=h,width=w,device=cairo_pdf) #save pdf
} else if(fext(.outPF)=='png') {
  ggsave(.outPF,plot=p,height=h,width=w,type='cairo')
}


#---- Finalize script ----#

if(!.test) {
  suppressWarnings(
    suppressPackageStartupMessages({
      library(git2r)
      library(uuid)
  }))
  
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