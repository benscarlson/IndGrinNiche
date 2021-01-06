#!/usr/bin/env Rscript --vanilla

'
fig_metrics_hist
mode - is spec, nested, or clust
sesid - can be csl of session ids
stat - session id containing the statistic

Usage:
fig_metrics_hist.r <sesid> <mode> <out> [--stat=<stat>] [--labels=<labels>] [--seed=<seed>] [-t] 
fig_metrics_hist.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-e --stat=<stat>
-l --labels=<labels>
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
  
  .sesid <- c('full_hvs','full_ci','full_bg_buf')
  #.sesid <- 'full_ci'
  .mode <- 'spec'
  .stat <- c('5axes2000pts1') #,color='red'
  .labels <- c('Null','Sampling dist.','Background (buffer)')
  .outPF <- file.path(.wd,glue('figs/{.mode}_null_ci_bg.pdf'))
  
} else {
  library(docopt)
  library(rprojroot)
  
  ag <- docopt(doc, quoted_args=TRUE, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .sesid <- trimws(unlist(strsplit(ag$sesid,',')))
  .mode <- ag$mode
  .stat <- ag$stat
  .labels <- trimws(unlist(strsplit(gsub('\\"','',ag$labels),',')))# ifelse(is.null(ag$labels),NULL,)
  .outPF <- ifelse(isAbsolute(ag$out),ag$out,file.path(.wd,ag$out))
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed)) 

set.seed(.seed)
t0 <- Sys.time()

options(rgl.useNULL = TRUE) #hypervolume loads rgl, this makes sure it doesn't open x11 session

source(rd('src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(DBI)
    library(RSQLite)
  }))

source(rd('src/funs/breezy_funs.r'))
source(rd('src/funs/themes.r'))

theme_set(theme_eda)

#---- Local parameters ----
.dbPF <- '~/projects/whitestork/results/stpp_models/huj_eobs/data/database.db'

#---- Load control files ----#

#---- Initialize database ----#
db <- dbConnect(RSQLite::SQLite(), .dbPF)
invisible(assert_that(length(dbListTables(db))>0))

nsettb <- tbl(db,'niche_set_stats')

#---- Load data ----#

#TODO: can significanly streamline this code.
if(.mode=='spec') {
   title <- '(a)'
   xlab <- 'Specialization'
   
   gdat <- nsettb %>% 
     filter(ses_id %in% .sesid) %>%
     mutate(value=1-rini) %>%
     select(ses_id,rep,niche_set,value) %>%
     as_tibble
   
   stdat <- nsettb %>%
     filter(ses_id %in% .stat & (is.na(rep) | rep==1)) %>%
     mutate(value=1-rini) %>%
     select(ses_id,rep,niche_set,value) %>%
     as_tibble
   
} else if(.mode=='nested') {
  title='(b)'
  xlab='Nestedness'
  
  gdat <- nsettb %>% 
    filter(ses_id %in% .sesid) %>%
    select(ses_id,rep,niche_set,value=nestedness) %>%
    as_tibble
  
  stdat <- nsettb %>%
    filter(ses_id %in% .stat & (is.na(rep) | rep==1)) %>%
    select(ses_id,rep,niche_set,value=nestedness) %>%
    as_tibble
  
} else if(.mode=='clust') {
  title='(c)'
  xlab='Clustering'
  
  gdat <- nsettb %>% 
    filter(ses_id %in% .sesid) %>%
    select(ses_id,rep,niche_set,value=clust_w) %>%
    as_tibble
  
  stdat <- nsettb %>%
    filter(ses_id %in% .stat & (is.na(rep) | rep==1)) %>%
    select(ses_id,rep,niche_set,value=clust_w) %>%
    as_tibble
}

if(!is.null(.labels)) {
  gdat <- gdat %>%
    mutate(ses_id=factor(ses_id,levels=.sesid,labels=.labels))
}

#first <- gdat %>% filter(rep==1 & ses_id=='full_hvs')

#I think this is how to calculate bootstrap p-value
# nul <- d %>% filter(rep!=1 & niche_set=='beuster-2015') %>% pluck('value')
# st <- d %>% filter(rep==1 & niche_set=='beuster-2015') %>% pluck('value')
# length(nul[nul >= st])/length(nul) # proportion of nul values that are >= the statistic

p <- gdat %>%
  filter(rep!=1) %>%
  ggplot(aes(x=value,color=ses_id,fill=ses_id)) +
  geom_histogram(alpha=0.4) + #aes(y=stat(count) / sum(count))
  geom_vline(data=stdat,mapping=aes(xintercept=value),color='red',linetype='longdash') +
  #geom_density(..scaled..) +
  #lims(x=c(0,1.5)) +
  #scale_color_manual(values=c('#373F51','#60AFFF'),aesthetics=c('color','fill')) +
  facet_wrap(vars(niche_set)) +
  #facet_grid(vars(niche_set),vars(metric),scales='free') +
  labs(
    x=xlab,y='Frequency',
    color='Null distribution for:',
    fill='Null distribution for:',
    title=title
    #subtitle='2000 points per individual niche, 100 randomized repetitions',
    #caption='Non-randomized value (red line) and distribution for randomized data (density)'
    )

dir.create(dirname(.outPF),recursive=TRUE,showWarnings=FALSE)

h=4.5; w=8
if(fext(.outPF)=='pdf') {
  ggsave(.outPF,plot=p,height=h,width=w,device=cairo_pdf) #save pdf
} else if(fext(.outPF)=='png') {
  ggsave(.outPF,plot=p,height=h,width=w,type='cairo')
}
