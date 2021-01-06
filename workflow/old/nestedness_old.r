#!/usr/bin/env Rscript

set.seed(5326)

'
#Calculates nestedness for each pair and mean nestedness per niche set
# Stores results in the database

Usage:
nestedness <hvjob>
nestedness (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
' -> doc

source('~/projects/rsf/src/startup.r')

spsm(library(DBI))
spsm(library(docopt))
spsm(library(RSQLite))

if(interactive()) {
  .pd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
  .hvjob <- '5axes2000pts1'
} else {

  ag <- docopt(doc, version = '0.1\n')
  .pd <- getwd()
  .hvjob <- ag$hvjob
}

.dbP <- file.path(file.path(.pd,"data/database.db"))

#---- load data ----#
nsets <- read_csv(file.path(.pd,'niche_sets.csv'),col_types=cols()) %>% filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.pd,'niches.csv'),col_types=cols()) %>% filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

db <- DBI::dbConnect(RSQLite::SQLite(), .dbP)

npw <- tbl(db,'pairwise') %>% filter(hv_job==.hvjob)
nind <- tbl(db,'indiv_vol') %>% filter(hv_job_name==.hvjob)

#----
#---- Pairwise nestedness ----#
#----

#join to nind twice to get volumns for both niches
dat <- npw %>%
  left_join(nind %>% select(niche_name,indiv_vol),by=c('niche_name1'='niche_name')) %>%
  rename(vol1=indiv_vol) %>%
  left_join(nind %>% select(niche_name,indiv_vol),by=c('niche_name2'='niche_name')) %>%
  rename(vol2=indiv_vol) %>%
  as_tibble %>%
  left_join(niches %>% select(niche_name,niche_set),by=c('niche_name1'='niche_name'))

#Old method of using map, can delete. Using pmap instead
# dat %>%
#   select(-pairwise_id) %>%
#   nest(data=c(hv_intr_vol,vol1,vol2)) %>% #kinda hacky to get map to work
#   mutate(nestedness=map_dbl(data,~{.$hv_intr_vol/min(.$vol1,.$vol2)})) %>%
#   unnest(data) #%>% View

nestdf <-  dat %>%
  mutate(nestedness=pmap_dbl(list(hv_intr_vol,vol1,vol2),
    function(hv_intr_vol,vol1,vol2) {
      hv_intr_vol/min(vol1,vol2)
    }))

# update pairwise table with pairwise nestedness metrics
for(i in seq_len(nrow(nestdf))) {
  #i <- 1
  #could just update based on pairwise_id, but put in other values just to be sure
  row <- nestdf[i,]
  sql <- glue_sql(
    "update pairwise 
    set nestedness={row$nestedness} 
    where pairwise_id={row$pairwise_id}
    and niche_name1={row$niche_name1} 
    and niche_name2={row$niche_name2}
    and hv_job={row$hv_job}",.con=db)
  
  af <- dbExecute(db,sql)
  
  #should update exactly 1 row, if not provide warning
  if(af != 1) message(glue('Warning, update for {row$niche_name1}, {row$niche_name2} affected {af} rows'))
}

#----
#---- Niche set nestedness
#----

sqldat <- nestdf %>%
  group_by(hv_job,niche_set) %>%
  summarize(nestedness=mean(nestedness)) %>%
  rename(hv_job_name=hv_job)


# Update database
for(i in seq_len(nrow(sqldat))) {
  #i <- 1
  row <- sqldat[i,]
  sql <- glue_sql(
    "update niche_set_vol 
    set nestedness={row$nestedness} 
    where niche_set={row$niche_set} 
    and hv_job_name={row$hv_job_name}",.con=db)
  af <- dbExecute(db,sql)
  
  #should update exactly 1 row, if not provide warning
  if(af != 1) message(glue('Warning, update for {row$niche_set} affected {af} rows'))
}
  
message('Script complete')
