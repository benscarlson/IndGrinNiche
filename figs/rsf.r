#!/usr/bin/env Rscript

#-----
#TODO: 
# 
#-----

'
Usage:
rsf.r <pop> <year> <mod> <out> [--seed=<seed>] [--mode=<mode>]
rsf.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-m --mode=<mode>  Pass in "supp" to output figs in supplement mode
' -> doc

#---- Input Parameters ----#
if(interactive()) {
  library(here)
  
  .wd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
  .script <- 'src/script_template.r' #Currently executing script
  .seed <- NULL
  rd <- here
  
  .year <- 2013
  .pop <- 'loburg'
  .modName <- 'mod4'
  .outPF <- file.path(.wd,'figs/ms/3',glue('3_rsf_{.pop}_{.year}.png'))
  
} else {
  library(docopt)
  library(rprojroot)
  
  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .modName <- ag$mod
  .year <- as.numeric(ag$year)
  .pop <- ag$pop
  .outPF <- ag$out
  .mode <- ag$mode
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(.seed)
t0 <- Sys.time()

source(rd('src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
  library(DBI)
  library(ggiraphExtra)
  library(patchwork)
  library(RSQLite)
  library(scales)
  library(uuid)
}))

source(rd('src/funs/funs.r'))
source(rd('src/funs/themes.r'))
theme_set(theme_ms)

#---- Parameters ----#
.runid <- UUIDgenerate()
.parPF <- file.path(.wd,"run_params.csv")
.dbPF <- file.path(.wd,'data/database.db')

#---- load data ----#

db <- DBI::dbConnect(RSQLite::SQLite(), .dbPF)
summ <- tbl(db,'model_summary') %>% filter(mod_name==.modName)
indTb <- loadIndivtb()

nsets <- read_csv(file.path(.wd,'ctfs/niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'ctfs/niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set') %>%
  filter(year==.year & population==.pop)
envs <- read_csv(file.path(.wd,'envs.csv'),col_types=cols())

#---
#--- traditional windrose with colors for pos, neg, nonsignificant. error bars
#--- 

getPlot <- function(gdat,term,grid.mid,tag=NULL) {
  
  #cols <- c('neg'=muted('red'),'pos'=muted('blue'),'nsig'='white')
  cols <- c('neg'='firebrick2','pos'='royalblue1','nsig'='grey90')
  
  title <- ifelse(is.null(tag),term,glue('{tag} {term}'))
  
  p <- ggplot(gdat,aes(x=short_name,y=estimate,fill=class)) + #
    #geom_hline(yintercept=1,color='grey') +
    geom_col(color=NA) +
    #geom_crossbar(aes(ymin=conf.low,ymax=conf.high),width=0.5) +
    geom_hline(aes(yintercept=grid.mid,linetype='No selection'),color='grey') +
    geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=0.2) +
    scale_fill_manual(values=cols) +
    scale_linetype_manual(name=NULL,values='dashed') +
    guides(fill=FALSE) +
    #lims(y=c(0,1)) +
    theme(
      axis.text.x=element_text(size=7),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title=element_blank(),
      axis.line=element_blank(),
      legend.position=c(0,0.95),
      legend.text = element_text(size = 8),
      plot.title=element_text(hjust=0.5)) +
    coord_polar() +
    ggtitle(title); #plot(p)
  
  return(p)
}

dat <- summ %>% 
  filter(component %in% c('conf.low', 'estimate', 'conf.high')) %>%
  select(niche_name,term,component,value) %>% 
  as_tibble() %>%
  inner_join(niches %>% select(niche_set,niche_name,individual_id,year,population),by='niche_name') %>%
  inner_join(indTb %>% select(individual_id,short_name),by='individual_id')

grid.mid <- (0 - min(dat$value))/diff(range(dat$value))

ggdf <- dat %>%
  mutate(value=rescale(value)) %>% 
  spread(component,value) %>%
  mutate(class=case_when(
    conf.low >= grid.mid & conf.high >= grid.mid ~ 'pos', #if conf limits > 0, then pos
    conf.low <= grid.mid & conf.high <= grid.mid ~ 'neg', #if conf limits < 0, then neg
    TRUE ~ 'nsig' #else non significant
  )) %>% #  filter(term=='dist2urban') 
  nest(gdat=-term) %>%
  left_join(envs %>% select(term=label,label_figure),by='term') %>%
  mutate(
    #tag=letters[row_number()],
    gg=pmap(list(gdat,label_figure,grid.mid),getPlot)) #tag,

#ggdf$gg[[2]]

plist <- ggdf$gg

#Only show individual names for the first panel
plist[-1] <- plist[-1] %>% map(function(gg) {
  gg + theme(axis.text.x=element_blank())
})

#Only show legend on the second panel
plist[-2] <- plist[-2] %>% map(function(gg) {
  gg + theme(legend.position = 'none')
})

#Doesn't work well
# layout <- '
# AABBCCDD
# #EEFFGG#'

p <- wrap_plots(plist,ncol=4)

if(!is.null(.mode) && .mode =='supp') { #note requires use of short-cicuit &&!
  p <- p + plot_annotation(
    title=glue('{str_to_title(.pop)} {.year}'),
    theme = theme(plot.title = element_text(size = 24)))
}

h=6; w=10
if(fext(.outPF)=='pdf') {
  ggsave(.outPF,plot=p,height=h,width=w,device=cairo_pdf) #save pdf
} else if(fext(.outPF)=='png') {
  ggsave(.outPF,plot=p,height=h,width=w,type='cairo')
}

#ggsave(.outPF,dpi=300,plot=p,height=6,width=10,type='cairo')

#---- Finalize script ----#
#Save all parameters to csv for reproducibility
#TODO: write this to a workflow database instead
saveParams(.parPF)

dbDisconnect(db)

message(glue('Script complete in {diffmin(t0)} minutes'))