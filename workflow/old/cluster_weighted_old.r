#!/usr/bin/env Rscript
# chmod 744 script_template.r #Use to make executable

'
Calculates global weighted cluster index for each niche set.
Saves results to database.

Usage:
cluster_weighted.r <hvjob> [--seed=<seed>]
cluster_weighted (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed

' -> doc

#---- Input Parameters ----#

if(interactive()) {
  .wd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
  .hvjob <- '5axes2000pts1'
  .seed <- NULL
} else {
  library(docopt)
  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .hvjob <- ag$hvjob
  .seed <- ag$seed
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(.seed)
t0 <- Sys.time()

library(here)
source(here('src/startup.r'))

spsm(library(DBI))
spsm(library(RSQLite))
spsm(library(tnet))
spsm(library(uuid))

source(here('src/funs/funs.r'))

#---- Parameters ----#

.runid <- UUIDgenerate()
.script <- here('src/workflow/cluster_weighted.r') #Currently executing script
.dbPF <- file.path(.wd,"data/database.db")
.parPF <- file.path(.wd,"run_params.csv")
.minOverlap <- 0.05
.measure <- 'am' #c("am", "gm", "ma", "mi")

#Save all parameters to csv for reproducibility
#TODO: write this to a workflow database instead
saveParams(.parPF)

#---- Load data ----#

db <- dbConnect(RSQLite::SQLite(), .dbPF)

npw <- tbl(db,'pairwise') %>% filter(hv_job==.hvjob)
nind <- tbl(db,'indiv_vol') %>% filter(hv_job_name==.hvjob)

nsets <- read_csv(file.path(.wd,'niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set') %>%
  mutate(niche_id=row_number()) #need a niche_id to make matrix for clust_w

#---- Perform analysis ----#

# Join to get niche_ids and niche_set
# Calculate pairwise jaccard
# Filter where overlap is above threshold
dat <- npw %>%
  as_tibble %>%
  left_join(
    niches %>% select(niche_name,niche_id1=niche_id,niche_set),
    by=c('niche_name1'='niche_name')) %>%
  left_join(
    niches %>% select(niche_name,niche_id2=niche_id), #already got niche_set with first join
    by=c('niche_name2'='niche_name')) %>%
  mutate(jrd=hv_intr_vol/hv_union_vol) %>% #TODO: can now pull directly from db
  filter(jrd >= .minOverlap) 

# Calculate niche_set level weighted clustering index
cldat <- dat %>%
  select(niche_set,i=niche_id1,j=niche_id2,w=jrd) %>%
  nest(data1=-niche_set) %>%
  mutate(data2=map(data1,~{
    rbind(.,data.frame(i=.$j,j=.$i,w=.$w)) #have to make symetric links for cluster_w to work
  })) %>%
  mutate(clust_w=map_dbl(data2,~{
    clustering_w(as.matrix(.),measure=.measure)
  })) %>%
  select(niche_set,clust_w) %>%
  mutate(hvjob=.hvjob) #%>% kable
  
#---- Save results to database ----#

dbExecute(db,'PRAGMA foreign_keys=ON')
dbBegin(db)

sql <- glue_sql('update niche_set_stats 
  set clust_w = $clust_w 
  where niche_set = $niche_set and hv_job_name = $hvjob')

rs <- dbSendStatement(db, sql) #parameter names should match column names
dbBind(rs,params=cldat)
rows <- dbGetRowsAffected(rs)
dbClearResult(rs)

#---- Finalize script ----#
if(nrow(cldat) == rows) {
  dbCommit(db)
} else {
  dbRollback(db)
  message('All rows did not update correctly. Transaction rolled back.')
}

dbDisconnect(db)

message(glue('Script complete in {diffmin(t0)} minutes'))
