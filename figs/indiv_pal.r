#!/usr/bin/env Rscript

'
This attaches a palette color to each individual

Usage:
indiv_pal <out> [--seed=<seed>] [-t]
indiv_pal (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run
' -> doc

#---- Input Parameters ----
if(interactive()) {
  library(here)
  .wd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .outPF <- file.path(.wd,'figs/ms/indiv_pal.csv')
  
} else {
  library(docopt)
  library(rprojroot)
  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .outPF <- ag$out
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(.seed)
t0 <- Sys.time()

source(rd('src/startup.r'))

spsm(library(git2r))
spsm(library(scales))

source(rd('src/funs/funs.r'))

#---- Parameters ----#
.runid <- UUIDgenerate()
.parPF <- file.path(.wd,"run_params.csv")

#---- Load data ----#

nsets <- read_csv(file.path(.wd,'niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

#---- Perform analysis ----#

niches %>%
  distinct(population,individual_id) %>%
  nest(data=-c(population)) %>%
  mutate(data=map(data,function(dat) {
    dat %>%
      mutate(color=hue_pal()(n()))
  })) %>%
  unnest(data) %>%
  select(individual_id,color) %>%
  write_csv(.outPF)
  
#---- Finalize script ----#

if(!.test) {
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
