#!/usr/bin/env Rscript --vanilla

# ==== Breezy setup ====

'
Template

Usage:
sex_niche_size.r  <sesid> <out> [-t] [--seed=<seed>]
sex_niche_size.r (-h | --help)

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

  .wd <- '~/projects/ms1/analysis/huj_eobs'
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .sesid <- '5axes2000pts1'
  #.datPF <- file.path(.wd,'data/dat.csv')
  .outPF <- file.path(.wd,'figs/myfig.png')
  
} else {
  library(docopt)
  library(rprojroot)

  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  #.list <- trimws(unlist(strsplit(ag$list,',')))
  #.datPF <- ifelse(isAbsolute(ag$dat),ag$dat,file.path(.wd,ag$dat))
  .sesid <- ag$sesid
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

source(rd('src/funs/auto/breezy_funs.r'))
source(rd('src/funs/auto/themes.r'))
theme_set(theme_eda)

#---- Local parameters ----#
.dbPF <- file.path(.wd,"data/database.db")
#.dbBrdPF <- file.path('~/projects/whitestork/src/db/db.sqlite')

#---- Load control files ----#
nsets <- read_csv(file.path(.wd,'ctfs/niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'ctfs/niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

#---- Initialize database ----#
db <- dbConnect(RSQLite::SQLite(), .dbPF)
invisible(assert_that(length(dbListTables(db))>0))

nind <- tbl(db,'niche_stats') %>% filter(ses_id==.sesid) %>% as_tibble

#db2 <- dbConnect(RSQLite::SQLite(), .dbBrdPF)
#invisible(assert_that(length(dbListTables(db2))>0))


#ind <- tbl(db2,'individual') %>% as_tibble
fran <- read_csv(file.path('~/projects/whitestork/data/derived/shay/franzmitters.csv'), 
                 col_types=cols())

#---- Load data ----#
#message('Loading data...')
# dat0 <- read_csv(.datPF,col_types=cols()) %>%
#   inner_join(niches %>% select(niche_set,niche_name),by='niche_name')

#====

#---- Perform analysis ----#

#fran file is strange b/c it sex is missing for individuals in some years but not others
#need to consolidate this information
indx <- fran %>% 
  select(individual_id,sex) %>% 
  mutate(sex=ifelse(sex=='?',NA,sex)) %>% 
  filter(!is.na(sex)) %>%
  distinct(individual_id, sex)

gdat <- niches %>%
  inner_join(indx,by='individual_id') %>%
  inner_join(
    nind %>% select(niche_name,niche_vol),
    by='niche_name')

p <- gdat %>%
    filter(!is.na(sex)) %>%
    #group_by(individual_id,sex) %>%
    #summarize(niche_vol=mean(niche_vol)) %>%
    mutate(sex=factor(sex,levels=c('F','M'))) %>%
  ggplot(aes(x=sex,y=log(niche_vol))) +
    geom_boxplot() +
  labs(x='Sex',
       y='log of niche volume')

# Counts of sample size
# gdat %>% filter(!is.na(sex)) #n=115
# gdat %>% filter(!is.na(sex)) %>%
#   group_by(individual_id,sex) %>%
#   summarize(num=n()) %>% nrow #n=44

#---- Save output ---#
dir.create(dirname(.outPF),recursive=TRUE,showWarnings=FALSE)

h=4; w=5
if(fext(.outPF)=='pdf') {
  ggsave(.outPF,plot=p,height=h,width=w,device=cairo_pdf) #save pdf
} else if(fext(.outPF)=='png') {
  ggsave(.outPF,plot=p,height=h,width=w,type='cairo')
}


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