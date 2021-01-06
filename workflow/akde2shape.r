#!/usr/bin/env Rscript

'
Load AKDE objects and saves 95% contours to a shapefile

Usage:
akde2shape <akde> <out>
akde2shape (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.

' -> doc

set.seed(5326)
t0 <- Sys.time()

# library(rprojroot)
# 
# root <- is_rstudio_project
# root$find_file("src/funs/funs.r")

source('~/projects/rsf/src/startup_script.r')
source('~/projects/rsf/src/funs/funs.r')
#source(root$find_file("src/funs/funs.r"))

spsm(library(ctmm))
spsm(library(sf))

#---- Functions ----
unchop_poly <- function(data, col) {
  mutate(data, {{col}} := map({{col}}, ~split(., seq_len(nrow(.))))) %>%
    unchop({{col}})
}
#----

#---- Parameters ----

if(interactive()) {
  .pd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
  .akdeP <- file.path(.pd,'ctmm/akde')
  .outP <- file.path(.pd,'ctmm/akde_contour/contours.shp')
} else {
  spsm(library(docopt))
  ag <- docopt(doc, version = '0.1\n')
  .pd <- getwd()
  .akdeP <- ag$akde
  .outP <- ag$out
}

#---- Load data ----
nsets <- read_csv(file.path(.pd,'niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.pd,'niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')
#----

message(glue('Loading {nrow(niches)} akde objects'))

polys0 <- niches %>%
  mutate(poly=map(niche_name,~{
    readRDS(file.path(.akdeP,glue('{.}.rds'))) %>%
      pluck('result') %>%
      SpatialPolygonsDataFrame.UD(level.UD=0.95,level=0.95) %>%
      st_as_sf %>%
      st_transform(4326) %>%
      mutate(name=as.character(name)) %>%
      return()
  })) %>%
  unchop_poly(poly) %>%
  mutate(name=map_chr(poly,'name')) #extract name of polygon so that join will work

#polys0$poly[[1]]$name #make sure name is a character and not factor

#Need to convert list column of sf objects to a single sf
#Here is what I could figure out. Use rbind to create sf out of the sf list column. Then, join other data to it.
#This isn't ideal because need to ensure that name is unique

polys <- do.call(rbind,polys0$poly) %>%
  left_join(polys0 %>% select(-poly),by='name') %>%
  separate(name,c('name','per','est'),sep=' ')

# polys %>%
#   filter(niche_name=='Agatha-2013') %>%
#   select(est) %>%
# ggplot() +
#   geom_sf(aes(color=est),fill=NA)

message('Saving shapefile')

polys %>%
  select(n_name=niche_name,per,est) %>% #shapefile can't store long names
  st_write(.outP,delete_layer=TRUE)

#con <- st_read(file.path(.pd,'ctmm/akde_contour/contours.shp'))
message(glue('Script complete in {diffmin(t0)} minutes'))
