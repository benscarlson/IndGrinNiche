#Calculates rini for individuals and niche sets and stores in database
spsm(library(DBI))
spsm(library(RSQLite))

#source('~/projects/rsf/src/scripts/funs.r')

#---- Parameters ----#
if(interactive()) {
  .pd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
} else {
  .pd <- getwd()
}

#---- Paths ----#
.hvjob <- '5axes2000pts1'
.dbP <- file.path(file.path(.pd,"data/database.db"))

#---- Load data ----#
nsets <- read_csv(file.path(.pd,'niche_sets.csv'),col_types=cols()) %>% filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.pd,'niches.csv'),col_types=cols()) %>% filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

db <- DBI::dbConnect(RSQLite::SQLite(), file.path(.pd,"data/database.db"))

nind <- tbl(db,'indiv_vol') %>% filter(hv_job_name==.hvjob)
nset <- tbl(db,'niche_set_vol') %>% filter(hv_job_name==.hvjob) 

#---- Cacluate rini per individual ----#

rini <- niches %>%
  inner_join(as_tibble(nind), by='niche_name') %>%
  inner_join(as_tibble(nset),by=c('niche_set','hv_job_name')) %>%
  mutate(rini=indiv_vol/niche_set_vol) %>%
  select(niche_set,niche_name,hv_job_name,rini)

# update individual table with individual rini

for(i in seq_len(nrow(rini))) {
  #i <- 1
  row <- rini[i,]
  sql <- glue_sql(
    "update indiv_vol 
    set rini={row$rini} 
    where niche_name={row$niche_name} 
    and hv_job_name={row$hv_job_name}",.con=db)
  
  af <- dbExecute(db,sql)
  
  #should update exactly 1 row, if not provide warning
  if(af != 1) message(glue('Warning, update for {row$niche_name} affected {af} rows'))
}

#---- Average RINI per niche set ----#
sqldat <- rini %>%
  group_by(hv_job_name,niche_set) %>%
  summarize(rini=mean(rini))

# update database 
for(i in seq_len(nrow(sqldat))) {
  #i <- 2
  row <- sqldat[i,]
  sql <- glue_sql(
    "update niche_set_vol 
    set rini={row$rini} 
    where niche_set={row$niche_set} 
    and hv_job_name={row$hv_job_name}",.con=db)
  af <- dbExecute(db,sql)
  
  #should update exactly 1 row, if not provide warning
  if(af != 1) message(glue('Warning, update for {row$niche_set} affected {af} rows'))
}
  


