#!/usr/bin/env Rscript

#TODO: I added code to plot confidence intervals, but we decided agasint them so I went back to the previous version of the 
# figure. This figure was found at v10/figs/4_metrics_time.png
# I should also revert back to the earlier version of this script.

'
Multipanel plot with specialization and nestedness over time, per population
Each year is shown with violin plots as well as line for mean

Usage:
metrics_time.r <hvjob> <out> [--seed=<seed>] [-t]
metrics_time.r (-h | --help)

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
  .script <- 'src/figs/ms/metrics_time.r'
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .hvjob <- '5axes2000pts1'
  .outPF <- file.path(.wd,'figs/ms/metrics_time_test.pdf')

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
  .outPF <- ag$out
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(.seed)
t0 <- Sys.time()

source(rd('src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(lubridate)
    library(DBI)
    library(patchwork)
    library(RSQLite)
  }))

source(rd('src/funs/funs.r'))
source(rd('src/funs/row_panel.r'))
source(rd('src/figs/metrics_time_funs.r'))
source(rd('src/funs/themes.r'))
theme_set(theme_ms)

#---- Parameters ----
.dbPF <- '~/projects/whitestork/results/stpp_models/huj_eobs/data/database.db'
.sesidCI <- 'full_ci'

#---- Initialize database ----#
db <- DBI::dbConnect(RSQLite::SQLite(), .dbPF)
invisible(assert_that(length(dbListTables(db))>0))

nind <- tbl(db,'niche_stats') %>% filter(ses_id==.hvjob)
nset <- tbl(db,'niche_set_stats') %>% filter(ses_id==.hvjob)
npw <- tbl(db,'pairwise') %>% filter(ses_id==.hvjob)
citb <- tbl(db,'metric_ci') %>% filter(ses_id==.sesidCI)
indTb <- loadIndivtb()

#---- Load data ----#

pal <- read_csv(file.path(.wd,'figs/ms/indiv_pal.csv'),col_types=cols())
nsets <- read_csv(file.path(.wd,'ctfs/niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'ctfs/niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set') %>%
  inner_join(indTb %>% select(individual_id,short_name),by='individual_id')


#---- 
#---- Analysis ----#
#----

#--
#-- Make dataframe for color and plot order
#--

#Uses indiv_vol to set order. To generalize, make order variable here instead and pass that in
#   mutate(order=row_number())
#TODO: could arrange by average indiv_vol
#
aesdf <- niches %>%
  distinct(population,individual_id,short_name) %>%
  inner_join(pal,by='individual_id') %>%
  nest(aesdat=-population)

#Setup CI data
cidf <- citb %>% 
  as_tibble %>%
  inner_join(
    niches %>%
      group_by(niche_set) %>%
      summarize(year=unique(year),population=unique(population)), 
    by='niche_set') %>% 
  select(-ses_id,-niche_set) %>%
  nest(cidat=-c(metric,population))

#--
#-- Specialization metric
#--

gginddf <- nind %>% 
  as_tibble %>%
  inner_join(niches %>% select(niche_name,year,population,short_name), by='niche_name') %>% #
  nest(nind=-population) %>%
  arrange(population) %>%
  inner_join(aesdf,by='population') %>%
  inner_join(
    cidf %>% filter(metric=='spec') %>% select(-metric),
    by='population') %>%
  mutate(gg=pmap(list(population,nind,aesdat,cidat),plotSpec))

#inddf$aesdat[[1]]
#gginddf$gg[[2]]

#--
#-- Nestedness metric ----#
#--

#since we know each pair is always in the same niche set, can just load the niche set using the first niche name
#also pair will always have the same year and population, so can load these at the same time
ggpwdf <- npw %>%
  as_tibble %>%
  left_join(
    niches %>% select(niche_set,niche_name,year,population,short_name1=short_name), #
    by=c('niche_name1'='niche_name')) %>%
  left_join(
    niches %>% select(niche_name,short_name2=short_name), #
    by=c('niche_name2'='niche_name')) %>%
  select(niche_set,year,population,niche_name1,niche_name2,nestedness,short_name1, short_name2,) %>% #
  mutate(pair=paste(short_name1,short_name2,sep='-')) %>%
  nest(npw=-population) %>%
  arrange(population) %>%
  inner_join(
    cidf %>% filter(metric=='nestedness') %>% select(-metric),
    by='population') %>%
  mutate(gg=pmap(list(population,npw,cidat),plotNested))

#ggpwdf$gg[[1]]

#--
#-- Clustering metric ----#
#--

#NOTE: error bars do not make sense here b/c so little variation
#TODO: ideally year should be stored in niche set file, not niches file
cldf <- nset %>% select(niche_set,clust_w) %>%
  as_tibble %>%
  inner_join(niches %>% distinct(niche_set,population,year),
             by='niche_set') %>%
  nest(cldat=-population) %>%
  arrange(population) %>%
  mutate(gg=pmap(list(population,cldat),plotClust))

#cldf$gg[[1]]

#---- Multipanel plot ----#
specrow <- rowPanel(gginddf$gg,ylab='Specialization',xhide=TRUE)
nestrow <- rowPanel(ggpwdf$gg,ylab='Nestedness',xhide=TRUE)
clrow <- rowPanel(cldf$gg,ylab='Clustering',xlab='Year',xhide=FALSE)

#-- set up row tags
# Put row tag into title field manually. Patchwork method takes too much space

specrow[[1]] <- specrow[[1]] + ggtitle(glue('a {specrow[[1]]$labels$title}'))
nestrow[[1]] <- nestrow[[1]] + ggtitle(glue('b'))
clrow[[1]] <- clrow[[1]] + ggtitle(glue('c'))

plist <- c(specrow,nestrow,clrow)
p <- wrap_plots(plist,nrow=3,ncol=3) +
  plot_layout(heights=c(2.5,2.5,1))

h=9; w=9
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
