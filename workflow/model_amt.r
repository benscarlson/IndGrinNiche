#TODO: I need to make this into a script

spsm <- suppressPackageStartupMessages

spsm(library(amt))
spsm(library(bencmisc))
spsm(library(DBI))
spsm(library(dplyr))
#spsm(library(forcats))
spsm(library(glue))
#spsm(library(raster))
spsm(library(readr))
spsm(library(RSQLite))
spsm(library(tidyr))

filter <- dplyr::filter
select <- dplyr::select

source('~/projects/rsf/src/scripts/funs.r')

#---- parameters ----
if(interactive()) {
  .pd <- '/Users/benc/projects/whitestork/results/stpp_models/huj_eobs'
} else {
  .pd <- getwd()
}

#pars <- loadParams(datName)
message(glue('Reading data sets from {.pd}'))
models <- read_csv(file.path(.pd,'models.csv'),col_types=cols()) %>%
  filter(as.logical(run))
niches <- read_csv(file.path(.pd,'niches.csv'),col_types=cols())
envs <- read_csv(file.path(.pd,'envs.csv'),col_types=cols())  %>%
  filter(as.logical(run))
dat0 <- read_csv(file.path(.pd,'data/obsbg_anno.csv'),col_types=cols())

dat0$log.dist2nest <- log(dat0$dist2nest)

db <- DBI::dbConnect(RSQLite::SQLite(), file.path(.pd,"data/database.db"))

#---- run the models ----#

for(j in 1:nrow(models)) {
  #j <- 1
  mod <- models[j,]

  f <- formula(mod$formula)
  message(glue('Fitting model {mod$formula}'))
  modvars <- all.vars(f[[3]])
  scalevars <- modvars[modvars!='stratum']
  
  ssf <- dat0 %>% 
    mutate(y=as.numeric(obs)) %>%
    select(niche_name,y,!!modvars) %>%
    mutate_at(.vars=scalevars,.funs=list(~as.vector(scale(.))))

  ptm <- proc.time()
    ssf2 <- ssf %>%
      group_by(niche_name) %>% 
      nest %>%
      mutate(fit = map(data, ~fit_ssf(.,f)))
  print(elapsedMinSec(ptm,ptm2<-proc.time()))
  
  #---- save the model objects
  ssf2 %>% select(-data) %>% saveRDS(file.path(.pd,'models',glue('{mod$short_name}.rds')))
  
  #---- save the model coefficients to a database
  ssf2 %>% mutate(coef = map(fit, ~ broom::tidy(.x$model))) %>%
    select(niche_name, coef) %>% 
    unnest %>%
    gather(component,value,-c(niche_name,term)) %>% 
    arrange(niche_name,term,component) %>%
    mutate(model_summary_id=NA,mod_name=mod$short_name) %>%
    select(model_summary_id,mod_name,niche_name,term,component,value) %>%
    dbAppendTable(db, "model_summary", .)
}

dbDisconnect(db)
message('Script Complete')

