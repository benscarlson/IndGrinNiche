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
  
  .wd <- '~/projects/ms1/analysis/rev2/null3_lbg15'
  .seed <- NULL
  .test <- TRUE
  rd <- here
  
  .sesid <- c('full_hvs','full_bg_buf','null3_lbg15')

  .mode <- 'spec'
  .stat <- '5axes2000pts1' #,color='red'
  .labels <- 1:3
  .outPF <- file.path(.wd,glue('figs/null3_lbg15/a_spec_null3.pdf'))
  
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

source(rd('src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(DBI)
    library(RSQLite)
  }))

#Source all files in the auto load funs directory
list.files(rd('src/funs/auto'),full.names=TRUE) %>%
  walk(source)

theme_set(theme_eda)

#---- Local parameters ----
.dbPF <- '~/projects/ms1/analysis/huj_eobs/data/database.db'

metrics <- list(spec='spec',nested='nestedness',clust='clust_w')

#---- Load control files ----#
nsets <- read_csv(file.path(.wd,'ctfs/niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)

#---- Initialize database ----#
invisible(assert_that(file.exists(.dbPF)))

db <- dbConnect(RSQLite::SQLite(), .dbPF)
invisible(assert_that(length(dbListTables(db))>0))

nsettb <- tbl(db,'niche_set_stats')

#---- Load data ----#

gdat <- nsettb %>% 
  filter(ses_id %in% .sesid & rep != 1) %>% #First rep always has unrandomized data, so don't pick that one
  select(ses_id,rep,niche_set,value=!!metrics[.mode][[1]]) %>%
  as_tibble %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

stdat <- nsettb %>%
  filter(ses_id %in% .stat & (is.na(rep) | rep==1)) %>%
  select(ses_id,rep,niche_set,value=!!metrics[.mode][[1]]) %>%
  as_tibble %>%
  inner_join(nsets %>% select(niche_set),by='niche_set') %>%
  mutate(niche_set=factor(niche_set,levels=nsets$niche_set,labels=nsets$label))

#TODO: I should combine xlab and metrics above into a list or dataframe
#       and title should be passed into the script as a parameter
if(.mode=='spec') {
   title <- '(a)'
   xlab <- 'Specialization'
} else if(.mode=='nested') {
  title='(b)'
  xlab='Nestedness'
  
} else if(.mode=='clust') {
  title='(c)'
  xlab='Clustering'

}

if(!is.null(.labels)) {
  gdat <- gdat %>%
    mutate(ses_id=factor(ses_id,levels=.sesid,labels=.labels))
}

p <- gdat %>%
    filter(rep!=1) %>%
    mutate(niche_set=factor(niche_set,levels=nsets$niche_set,labels=nsets$label)) %>%
  ggplot(aes(x=value,color=ses_id,fill=ses_id)) +
  geom_histogram(alpha=0.4) +
  geom_vline(data=stdat,mapping=aes(xintercept=value),color='red',linetype='longdash') +
  facet_wrap(vars(niche_set)) +
  labs(
    x=xlab,y='Frequency',
    color='Null model',
    fill='Null model',
    title=title)

dir.create(dirname(.outPF),recursive=TRUE,showWarnings=FALSE)

h=4.5; w=8
if(fext(.outPF)=='pdf') {
  ggsave(.outPF,plot=p,height=h,width=w,device=cairo_pdf) #save pdf
} else if(fext(.outPF)=='png') {
  ggsave(.outPF,plot=p,height=h,width=w,type='cairo')
}
