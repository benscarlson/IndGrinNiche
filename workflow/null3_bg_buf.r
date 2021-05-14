#!/usr/bin/env Rscript --vanilla

# This script implements the breezy philosophy: github.com/benscarlson/breezy

#TODO: need to have a more robust way to bring in z-scaling values. Maybe base it on model run id?

# ==== Breezy setup ====

'

This POC updates the script to use an available distribution sampled according to buffers. It uses the background sample from the second null model.

Implements a null model that samples the available environment for each individual according to a population-level RSF
The sampling technique is inspired by uhcplots
https://github.com/aaarchmiller/uhcplots/blob/master/R/uhcsimstrat.R#L65


Usage:
null3_bg_buf.r <dat> <out> <mod> <npts> [--seed=<seed>] [-t]
null3_bg_buf.r (-h | --help)

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

  .wd <- '~/projects/ms1/analysis/rev2/null3_bg_buf'
  .seed <- NULL
  .test <- TRUE
  rd <- here::here
  
  .mod <- 'mod4'
  .npts <- 50
  .datPF <- file.path(.wd,'data/bg_dist_anno.csv')
  .outPF <- file.path(.wd,'data/ud2k.csv')
  
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
  
  .mod <- ag$mod
  .npts <- as.integer(ag$npts)
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
  }))

#Source all files in the auto load funs directory
list.files(rd('src/funs/auto'),full.names=TRUE) %>%
  walk(source)

#---- Local parameters ----#
.dbPF <- '~/projects/ms1/analysis/huj_eobs/data/database.db'
.zscalePF <- '~/projects/ms1/analysis/huj_eobs/zscale.csv'

#---- Load control files ----#
nsets <- read_csv(file.path(.wd,'ctfs/niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'ctfs/niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

#In order to make population-level means, need a list of all niches in the niche set
nset_niches <- read_csv(file.path(.wd,'ctfs/niches.csv'),col_types=cols()) %>% 
  select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

zscale <- read_csv(.zscalePF,col_types=cols())

#---- Initialize database ----#
invisible(assert_that(file.exists(.dbPF)))

db <- dbConnect(RSQLite::SQLite(), .dbPF)

invisible(assert_that(length(dbListTables(db))>0))

#---- Load data ----#
message('Loading data...')

tic()
dat0 <- read_csv(.datPF,col_types=cols()) 
toc()

dat <- dat0 %>%
  inner_join(niches %>% select(niche_set,niche_name),by='niche_name')

#Use only for testing
#dat <- dat %>% group_by(niche_name) %>% sample_n(500) %>% ungroup

msum0 <- tbl(db,'model_summary') %>% 
  filter(mod_name==.mod & component=='estimate') %>% as_tibble %>%
  inner_join(nset_niches %>% select(niche_set,niche_name),by='niche_name')

#====

#---- Perform analysis ----#

# 1) coefficents are transformed (since glm) and on the log scale
# 2) normally we would run the regression equation then apply exp() to the response (see uhcsimstrat)
# 3) but we need to take the mean in order to have a population level RSF. So, do exp, take the mean, then take log to put back on the log scale
betas <- msum0 %>%
  group_by(niche_set,term) %>%
  summarize(value=log(mean(exp(value))))

#Need to streamline this
terms <- unique(betas$term)
#terms_a <- terms[terms!='pct_tree:ndvi'] #Can't create interaction column until after scaling
terms_a <- terms[!terms %in% c('pct_tree:ndvi','log.dist2nest')]
terms_b <- terms[terms != 'pct_tree:ndvi'] #we scale across all but the interaction

#Mimic creation of the design matrix.
#A more general way to do this would be to use model.matrix, but that has some issues
#create log.dist2nest, scale, and create interaction
#zscale contains mean and sd from the original dataset
#The interaction term is computed as part of the model function call. So, this has to happen *after* scaling
#Also, original models did complete.cases after scaling, so do that as well here. Shouldn't matter though.
ad <- dat %>%
  mutate(log.dist2nest=log(dist2nest)) %>%
  select(niche_name,niche_set,!!terms_b) %>%
  mutate(across(!!terms_b,~{
      v <- zscale %>% filter(var==cur_column())
      (. - v$mean)/v$sd
  }),
  'pct_tree:ndvi'=pct_tree*ndvi) %>%
  filter(complete.cases(.))

#Apply regression function to each location
adw <- ad %>%
  nest(data=-niche_set) %>%
  inner_join(betas %>% nest(betas=-niche_set),by='niche_set') %>%
  mutate(wx_s=map2(.x=data,.y=betas,~{
    .y <- .y %>% filter(term %in% terms) #%>% print
    .x <- .x %>% select(!!terms)

    #make sure coefficient order matches
    assert_that(all.equal(.y$term,colnames(.x)))
    
    (as.matrix(.x) %*% .y$value) %>% 
      as.numeric %>%
      exp
      
  })) %>%
  select(-betas) %>%
  unnest(cols=c(data,wx_s))

#Sample the available (and used) distribution according to the rsf
#This will generate the used distribution
#Assume here that .npts should be < n(), so we can use sample_n. 
message('Sampling background using population rsf...')
tic()
sdat <- adw %>% 
  group_by(niche_name) %>%
  slice_sample(n=.npts,weight_by=wx_s) %>%
  ungroup %>%
  select(niche_set,niche_name,!!terms,-log.dist2nest,-'pct_tree:ndvi')
toc()

#---- Save output ---#
message(glue('Saving to {.outPF}'))

dir.create(dirname(.outPF),recursive=TRUE,showWarnings=FALSE)

sdat %>% write_csv(.outPF)

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

dbDisconnect(db)

message(glue('Script complete in {diffmin(t0)} minutes'))