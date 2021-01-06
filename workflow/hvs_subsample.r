#----
# This is the exact code from first part of hvs_indiv.r
# The goal is to save the exact subsampled dataset used in the original hypervolume estimation
#----

spsm <- suppressPackageStartupMessages

options(rgl.useNULL = TRUE) #hypervolume loads rgl, this makes sure it doesn't open x11 session

spsm(library(docopt))
spsm(library(hypervolume))
spsm(library(foreach))
spsm(library(glue))
spsm(library(dplyr))
spsm(library(readr))

select <- dplyr::select
filter <- dplyr::filter

'
Subsample data for input into hypervolume estimation

Usage:
hvs_subsample.r <dat> <npts> <axes> <out> [--parMethod=<parMethod>]
hvs_subsample.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-p --parMethod=<parMethod>  Either <mpi | mc>. If not passed in, script will run sequentially.

' -> doc

#Note docopt initializes parameters that are not passed in to NULL
ag <- docopt(doc, version = '0.1\n')

if(interactive()) {
  .pd <- '~/projects/ms1/analysis/backgrounds'
  .annoPF <- 'data/bg_2wk_lbg2015_anno.csv'
  .npts <- 2000
  .axes <- c('pct_tree','dist2urban','pct_bare','dist2forest','ndvi')
  .parMethod <- 'mc'
  .outPF <- file.path(.pd,'data/bg_2wk_lbg2015_2k.csv')
} else {
  .pd <- getwd()
  .annoPF <- ag$dat
  .npts <- as.integer(ag$npts)
  .axes <- trimws(unlist(strsplit(ag$axes,',')))
  .parMethod <- ifelse(is.null(ag$parMethod),'none',ag$parMethod) #need to set value b/c NULL will fail in if statement
  .outPF <- ag$out
}

set.seed(594)

#---- parameters ----#

#---- functions ----#
diffmin <- function(t) round(difftime(Sys.time(), t, unit = "min"),2)

#---- load data ----#
message('Loading data, takes awhile...')

niches <- read_csv(file.path(.pd,'ctfs/niches.csv'),col_types=cols()) %>% filter(as.logical(run))
dat0 <- read_csv(file.path(.pd,.annoPF),col_types=cols())
message('Loading data complete')

message('Preparing data...')
if('obs' %in% names(dat0)) {
  dat0 <- dat0 %>% filter(obs)
}
#sub-sample each niche
datGrp <- dat0  %>%
  inner_join(niches %>% select(niche_set,niche_name),by='niche_name') %>%
  select(niche_set,niche_name,one_of(.axes)) %>%
  filter(complete.cases(.)) %>%
  mutate_at(.vars=vars(.axes),.funs=list(~as.vector(scale(.)))) %>%
  group_by(niche_set,niche_name) 

#subsample niches that have > .npts
datSamp <- datGrp %>% filter(n() > .npts) %>% sample_n(.npts) %>% ungroup()
#keep all data from niches that have <= .npts
datFull <- datGrp %>% filter(n() <= .npts) %>% ungroup()

datSamp <- bind_rows(datSamp,datFull)
message('Preparing data complete')

datSamp %>% write_csv(.outPF)