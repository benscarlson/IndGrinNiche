#!/usr/bin/env Rscript

#-----
#TODO: 
# *Test to make sure colors matched to individuals correctly
# *When making color palette, could also order individuals by avg niche vol
# *orient NMDS plots so that core areas could be interpreted. i.e. do plot show populations use different core areas?
#   or do they use the same core areas but environmental space is rotated?
# *Y axis label of MDS should also be moved in. This is b/c title for nmds y axis is axis.title.y, 
# while text for metrics is
#  axis.text.y. So, this makes them non-horizontally aligned. Not sure what
#  can be done about this.
# * Should have some way to label the contours for eda. idea: keep one dataset for each
#   niche. Pick the one with highest y value (so it's part of the convex hull)
#   then label these.
#   for sf objects, have a look at fun.geometry, maybe can be used to point to point on line
#-----

'
Multipanel plot for one year, three pops, with
NMDS as convex hulls, RINI, Nestedness, Clustering metrics as violins

Usage:
niches <hvjob> <hr> <year> <zoom> <out> [-t] [--seed=<seed>] [--mode=<mode>]
niches (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run
-m --mode=<mode>  Pass in "supp" to output figs in supplement mode
' -> doc

#---- Input Parameters ----
if(interactive()) {
  library(here)
  .wd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
  .test <- TRUE
  .script <- 'src/figs/niches.r' #Currently executing script
  .seed <- NULL
  rd <- here
  
  .hvjob <- '5axes2000pts1'
  .year <- 2015
  .hrP <- file.path(.wd,'ctmm/akde_contour/contours.shp')
  .zoom <- c(11,10,10)
  .outPF <- file.path(.wd,'figs/ms/niches',glue('niches_{.year}.pdf'))
} else {
  library(docopt)
  library(rprojroot)
  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .test <- as.logical(ag$test)
  .eda <- as.logical(ag$eda)
  .script <-  thisfile()
  .seed <- ag$seed
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .hvjob <- ag$hvjob
  .year <- as.numeric(ag$year)
  .hrP <- ag$hr
  .zoom <- as.integer(trimws(unlist(strsplit(ag$zoom, split=","))))
  .outPF <- ag$out
  .mode <- ag$mode
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(.seed)
t0 <- Sys.time()

source(rd('src/startup.r'))

spsm(library(bencmisc)) #has getMapRetry
spsm(library(ctmm))
spsm(library(ggmap))
spsm(library(ggsn))
spsm(library(patchwork))
spsm(library(DBI))
spsm(library(ggConvexHull))
spsm(library(patchwork))
spsm(library(RSQLite))
spsm(library(stringr))
spsm(library(sf))

source(rd('src/funs/funs.r'))
source(rd('src/funs/themes.r'))
source(rd('src/figs/niches_funs.r'))
theme_set(theme_ms)

#---- Parameters ----#

.dbPF <- file.path(.wd,"data/database.db")
.ordP <- file.path(.wd,'hvs',.hvjob,'nmds1000_slim')

#---- load data ----#

db <- dbConnect(RSQLite::SQLite(), .dbPF)
nind <- tbl(db,'indiv_stats') %>% filter(hv_job==.hvjob)
#nset <- tbl(db,'niche_set_vol') %>% filter(hv_job_name==.hvjob)
npw <- tbl(db,'pairwise') %>% filter(hv_job==.hvjob)
nss <- tbl(db,'niche_set_stats') %>% filter(hv_job==.hvjob)
#indTb <- loadIndivtb()

pal <- read_csv(file.path(.wd,'figs/ms/indiv_pal.csv'),col_types=cols())
nsets <- read_csv(file.path(.wd,'niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set') %>%
  filter(year==.year)

polys0 <- st_read(.hrP,quiet=TRUE) %>%
  filter(est=='est') %>%
  select(niche_name=n_name) %>% #have to rename b/c shapefile
  inner_join(niches %>% select(niche_name,population,niche_set,individual_id),by='niche_name')

#---- 
#---- Analysis ----#
#----

#---- Make dataframe for color and plot order
#Uses indiv_vol to set order. To generalize, make order variable here instead and pass that in
#   mutate(order=row_number())
aesdf <- niches %>%
  inner_join(pal,by='individual_id') %>%
  inner_join(nind %>% select(niche_name,indiv_vol) %>% as_tibble,
             by='niche_name') %>%
  arrange(niche_set,indiv_vol) %>%
  nest(aesdat=-c(niche_set,population))

#---
#--- Plot of AKDE ---#
#---

polygg <- polys0 %>%
  nest(data=-c(population,niche_set)) %>%
  inner_join(aesdf,by=c('niche_set','population')) %>%
  arrange(population) %>%
  mutate(cen=map(data,~{ #get the centroid of each population
    pt <- st_transform(.,crs=3035) %>%
      st_combine %>%
      st_centroid %>%
      st_transform(crs=4326)
    
    cen <- c( #need to make this centroid object because getMapRetry expects it
      X=pt[[1]][1],
      Y=pt[[1]][2])
    class(cen) <- 'centroid'
    
    return(cen)
  }),
  zoom=.zoom,
  tag=letters[row_number()]
  ) %>%
  mutate(gg=pmap(list(tag,population,data,cen,zoom,aesdat),plotMap))
#polygg$gg[[1]];polygg$gg[[2]];polygg$gg[[3]]
#2013: c(10,9,9)
#2015: c(11,10,10)

#---
#--- Plot of NMDS ---#
#---

#Get the mds data
ords <- niches %>%
  distinct(niche_set) %>%
  mutate(ord=map(niche_set,~{
    tryCatch(
      readRDS(file.path(.ordP,glue('{.}.rds'))),
      error=function(e) NULL) %>%
      return()
  }),
  stress=map_dbl(ord,~{pluck(.,'stress',.default=NA)}))

mdsdf <- ords %>%
  mutate(ordtbl=map(ord,~{
    .$points %>% 
      as_tibble(rownames='niche_name')
  })) %>%
  select(niche_set,ordtbl) %>% unnest(ordtbl) %>%
  mutate(niche_name=str_extract(niche_name,"\\w+\\-\\d+")) %>%
  left_join(niches %>% distinct(niche_set,population),by='niche_set') %>%
  nest(data=-c(niche_set,population))

# Create plot panels
mdsgg <- mdsdf  %>%
  inner_join(aesdf,by=c('niche_set','population')) %>%
  arrange(population) %>%
  mutate(gg=pmap(list(data,aesdat),plotMDS))

#mdsgg$gg[[1]]

#---
#--- Plots of RINI and Nested ---#
#---

spec <- nind %>% 
  as_tibble %>%
  inner_join(niches %>% select(niche_name,population), by='niche_name') %>%
  mutate(metric='spec',spec=1-rini) %>% #convert to long format, since only one category
  select(population,metric,value=spec)

#since we know each pair is always in the same niche set, can just load the niche set using the first niche name
#also pair will always have the same year and population, so can load these at the same time
pw <- npw %>%
  as_tibble %>%
  inner_join(
    niches %>% select(niche_name,population),
    by=c('niche_name1'='niche_name')) %>%
  select(population,nestedness) %>%
  pivot_longer(nestedness,names_to='metric',values_to='value')

gdat <- bind_rows(spec,pw) %>%
  mutate(metric=factor(metric,levels=c('nestedness','spec'),
                       labels=c('Nestedness','Specialization')))

#TODO: verify 1-mean(rini) is same as mean(1-rini)
nsdf <- nss %>% as_tibble %>%
  inner_join(niches %>% distinct(niche_set,population),by='niche_set') %>%
  mutate(spec=1-rini) %>%
  select(population,spec,nestedness,clust_w) %>%
  pivot_longer(c(spec,nestedness,clust_w),names_to='metric',values_to='value') %>%
  mutate(metric=factor(metric,levels=c('clust_w', 'nestedness','spec'),
                       labels=c('Clustering', 'Nestedness','Specialization'))) %>%
  nest(nsdat=-population)
  
  
#Use this line to ignore nestedness data
# gdat <- spec %>%
#   mutate(metric=factor(metric,levels=c('spec'),labels=c('Specialization')))

violingg <- gdat %>%
  nest(data=c(metric,value)) %>%
  inner_join(nsdf,by='population') %>%
  arrange(population) %>%
  mutate(gg=pmap(list(population,data,nsdat),plotViolin))

#violingg$gg[[1]]

#----
#---- Assemble multipanel plot ----#
#----

#-- Formatting for individual plots

polygg$gg[[1]] <- polygg$gg[[1]] + theme(axis.title.x=element_blank())

polygg$gg[[2]] <- polygg$gg[[2]] + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

polygg$gg[[3]] <- polygg$gg[[3]] + 
  theme(axis.title=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

mdsgg$gg[[1]]  <- mdsgg$gg[[1]] +
  theme(axis.title.x=element_blank())

mdsgg$gg[[2]]  <- mdsgg$gg[[2]] +
  theme(axis.title.y=element_blank())

mdsgg$gg[[3]]  <- mdsgg$gg[[3]] +
  theme(axis.title=element_blank())


violingg$gg[[2]] <- violingg$gg[[2]] +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

violingg$gg[[3]] <- violingg$gg[[3]] +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#-- set up row tags
# Put row tag into title field manually. Patchwork method takes too much space
polygg$gg[[1]] <- polygg$gg[[1]] + ggtitle(glue('a {polygg$gg[[1]]$labels$title}'))
mdsgg$gg[[1]] <- mdsgg$gg[[1]] + ggtitle(glue('b'))
violingg$gg[[1]] <- violingg$gg[[1]] + ggtitle('c')

plist <- c(polygg$gg,mdsgg$gg,violingg$gg)

p <- wrap_plots(plist,nrow=3,ncol=3) #+

#Need to use isTRUE here otherwise need to use: 
#  !is.null(.mode) && .mode =='supp' (note use of short-circuit &&)
if(isTRUE(.mode =='supp')) {
  p <- p + plot_annotation(
    title=.year,
    theme = theme(plot.title = element_text(size = 24)))
}

h=10; w=9
if(fext(.outPF)=='pdf') {
  ggsave(.outPF,plot=p,height=h,width=w,device=cairo_pdf) #save pdf
} else if(fext(.outPF)=='png') {
  ggsave(.outPF,plot=p,height=h,width=w,type='cairo')
}

#---- Finalize script ----#

#Save all parameters to csv for reproducibility
if(!.test) {
  spsm(library(git2r))
  spsm(library(uuid))
  
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
