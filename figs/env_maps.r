#!/usr/bin/env Rscript --vanilla
# chmod 744 script_template.r #Use to make executable

# This script implements the breezy philosophy: github.com/benscarlson/breezy

#Note: this poc has been promoted to figs/env_maps.r

# ==== Breezy setup ====

#Need bounding box of each population. I might have that somewhere. o/w use hr shapefiles
#Need local copies of env rasters. I might also have this somewhere
#Need to make a composite of NDIV on gee
#So maybe a 5x3 figure? One row for each var, one column for each population

#Looks like I have data on disk that cover loburg in huj_eobs/layers. dist2forest.tif, pct_bare_30m.tif, pct_tree_30m.tif

'
Template

Usage:
env_maps.r <lon> <lat> <buf> <out> [--seed=<seed>] [-b] [-t]
script_template (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run, will not save output parameters or commit to git
' -> doc

#---- Input Parameters ----#
if(interactive()) {
  library(here)

  .wd <- '~/projects/ms1/analysis/rev2/env_maps'
  .seed <- NULL
  .test <- TRUE
  rd <- here::here
  
  .cen <- c(11.75705,52.95542)
  .buf <- 20000 #meters
  .outP <- file.path(.wd,'figs/env_panels')
  
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
  
  source(rd('src/funs/input_parse.r'))
  
  .cen <- as.numeric(c(ag$lon,ag$lat))
  .buf <- as.numeric(ag$buf)
  .outP <- makePath(ag$out)
}

# |population |        X|        Y|
#   |:----------|--------:|--------:|
#   |loburg     | 12.05762| 52.13555|
#   |dromling   | 11.03707| 52.44954|
#   |beuster    | 11.75705| 52.95542|

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(.seed)
t0 <- Sys.time()

source(rd('src/startup.r'))

suppressWarnings(
  suppressPackageStartupMessages({
    library(ggsn)
    library(sf)
    library(RStoolbox)
    library(raster)
  }))

#Source all files in the auto load funs directory
list.files(rd('src/funs/auto'),full.names=TRUE) %>%
  walk(source)

theme_set(theme_map)

#---- Local parameters ----#

.layersP <- '/Volumes/WD4TB/projects/ms1/data/derived/layers/'
.treePF <- file.path(.layersP,'pct_tree_utm32n.tif')
.barePF <- file.path(.layersP,'pct_bare_utm32n.tif')
.distforPF <- file.path(.layersP,'dist2forest_utm32n.tif')
.disturbanPF <- file.path(.layersP,'dist2urban_utm32n.tif')
.ndviMeanPF <- file.path(.layersP,'ndvi_mean_utm32n.tif')
.ndviCVPF <- file.path(.layersP,'ndvi_cv_utm32n.tif')

#---- Load control files ----#
# nsets <- read_csv(file.path(.wd,'ctfs/niche_sets.csv'),col_types=cols()) %>% 
#   filter(as.logical(run)) %>% select(-run)
# niches <- read_csv(file.path(.wd,'ctfs/niches.csv'),col_types=cols()) %>% 
#   filter(as.logical(run)) %>% select(-run) %>%
#   inner_join(nsets %>% select(niche_set),by='niche_set')

#---- Load data ----#
message('Loading data...')

tree <- raster(.treePF)
bare <- raster(.barePF)
distfor <- raster(.distforPF)
durban <- raster(.disturbanPF)
ndvim <- raster(.ndviMeanPF)
ndvicv <- raster(.ndviCVPF)

dir.create(.outP,showWarnings=FALSE,recursive=TRUE)
#====

#---- Perform analysis ----#


#--- Use the supplied centroid and make a square buffer
#Convert to utm
pt <- st_sfc(st_point(.cen),crs=4326) %>% st_transform(32632) %>% st_coordinates %>% as.numeric
#xmin, xmax, ymin, ymax
ext <- extent(c(pt[1] + c(-.buf,.buf), pt[2] + c(-.buf,.buf)))

#Make the bb so that scalebar displays correctly
b <- ext %>% st_bbox(crs=32632)
bbdf <- data.frame(long=c(b['xmin']+1000,b['xmax']),lat=c(b['ymin']+3000,b['ymax']))

#---- Tree ----#

tree2 <- crop(tree,ext,snap='out')

p <- ggR(tree2, geom_raster=TRUE) +
  scale_fill_gradientn(name=NULL, colours = rev(terrain.colors(100))) +
  ggtitle('Percent Tree') +
  ggsn::scalebar(data=bbdf,
                 dist = 5, dist_unit='km',transform = FALSE, location='bottomleft',
                 height=0.03, st.size=3, st.dist=0.05)

ggsave(file.path(.outP,'pct_tree.pdf'),plot=p,device=cairo_pdf) #save pdf #height=h,width=w,

#---- Bare ----#
bare2 <- crop(bare,ext,snap='out')

barePal <- "#F2F2F2FF,#F2F0F0FF,#F2EEEEFF,#F2ECECFF,#F2EBEAFF,#F2E9E8FF,#F2E7E6FF,#F2E5E4FF,#F1E3E2FF,#F1E2E0FF,#F1E0DEFF,#F1DEDCFF,#F1DDDAFF,#F1DBD8FF,#F1D9D7FF,#F1D8D5FF,#F1D6D3FF,#F1D5D1FF,#F0D3CFFF,#F0D2CDFF,#F0D1CBFF,#F0CFC9FF,#F0CEC7FF,#F0CDC5FF,#F0CBC3FF,#F0CAC1FF,#F0C9BFFF,#F0C8BDFF,#EFC7BBFF,#EFC5B9FF,#EFC4B7FF,#EFC3B5FF,#EFC2B3FF,#EFC1B1FF,#EFC0AFFF,#EFBFADFF,#EFBEACFF,#EFBEAAFF,#EEBDA8FF,#EEBCA6FF,#EEBBA4FF,#EEBAA2FF,#EEBAA0FF,#EEB99EFF,#EEB89CFF,#EEB89AFF,#EEB798FF,#EEB696FF,#EDB694FF,#EDB593FF,#EDB591FF,#EDB48FFF,#EDB48DFF,#EDB38BFF,#EDB389FF,#EDB387FF,#EDB285FF,#EDB283FF,#ECB281FF,#ECB27FFF,#ECB17EFF,#ECB17CFF,#ECB17AFF,#ECB178FF,#ECB176FF,#ECB174FF,#ECB172FF,#ECB170FF,#EBB16EFF,#EBB16CFF,#EBB16BFF,#EBB169FF,#EBB167FF,#EBB165FF,#EBB263FF,#EBB261FF,#EBB25FFF,#EBB25DFF,#EAB35CFF,#EAB35AFF,#EAB358FF,#EAB456FF,#EAB454FF,#EAB552FF,#EAB550FF,#EAB64FFF,#EAB64DFF,#EAB74BFF,#E9B749FF,#E9B847FF,#E9B945FF,#E9B943FF,#E9BA42FF,#E9BB40FF,#E9BB3EFF,#E9BC3CFF,#E9BD3AFF,#E9BE38FF,#E8BF36FF,#E8C035FF,#E8C133FF,#E8C231FF,#E8C32FFF,#E8C42DFF,#E8C52BFF,#E8C62AFF,#E8C728FF,#E8C826FF,#E7C924FF,#E7CA22FF" %>%
  str_split(',') %>% unlist

p <- ggR(bare2, geom_raster=TRUE, stretch='hist') + #
  scale_fill_gradientn(name=NULL, colours = barePal) +
  labs(title='Percent bare',caption='A histogram stretch has been applied to this layer.') +
  ggsn::scalebar(data=bbdf,
    dist = 5, dist_unit='km',transform = FALSE, location='bottomleft',
    height=0.03, st.size=3, st.dist=0.05)

ggsave(file.path(.outP,'pct_bare.pdf'),plot=p,device=cairo_pdf)

#---- Distance to forest ----#
distfor2 <- crop(distfor,ext,snap='out')

p <- ggR(distfor2, geom_raster=TRUE,stretch='log') + #, stretch='hist'
  scale_fill_gradientn(name=NULL, colours = terrain.colors(100)) +
  labs(title='Distance to forest',caption='A log stretch has been applied to this layer.') +
  ggsn::scalebar(data=bbdf,
                 dist = 5, dist_unit='km',transform = FALSE, location='bottomleft',
                 height=0.03, st.size=3, st.dist=0.05)

ggsave(file.path(.outP,'dist2forest.pdf'),plot=p,device=cairo_pdf)

#---- Distance to urban ----#
durban2 <- crop(durban,ext,snap='out')

p <- ggR(durban2, geom_raster=TRUE,stretch='log') + #, stretch='hist' 
  scale_fill_gradient(name=NULL, low='red',high='white') +
  labs(title='Distance to urban',caption='A log stretch has been applied to this layer.') +
  ggsn::scalebar(data=bbdf,
                 dist = 5, dist_unit='km',transform = FALSE, location='bottomleft',
                 height=0.03, st.size=3, st.dist=0.05)

ggsave(file.path(.outP,'dist2urban.pdf'),plot=p,device=cairo_pdf)

#---- NDVI Mean ----#
ndvim2 <- crop(ndvim,ext,snap='out')

p <- ggR(ndvim2, geom_raster=TRUE, stretch='hist' ) + #
  scale_fill_gradientn(name=NULL, colours = rev(terrain.colors(100))) +
  labs(title='Mean NDVI',caption='A histogram stretch has been applied to this layer.') +
  ggsn::scalebar(data=bbdf,
                 dist = 5, dist_unit='km',transform = FALSE, location='bottomleft',
                 height=0.03, st.size=3, st.dist=0.05)

ggsave(file.path(.outP,'ndvi_mean.pdf'),plot=p,device=cairo_pdf)

#---- NDVI CV ----#
ndvicv2 <- crop(ndvicv,ext,snap='out')

p <- ggR(ndvicv2, geom_raster=TRUE, stretch='hist' ) + #
  scale_fill_gradientn(name=NULL, colours = rev(terrain.colors(100))) +
  labs(title='CV of NDVI',caption='A histogram stretch has been applied to this layer.') +
  ggsn::scalebar(data=bbdf,
                 dist = 5, dist_unit='km',transform = FALSE, location='bottomleft',
                 height=0.03, st.size=3, st.dist=0.05)

ggsave(file.path(.outP,'ndvi_cv.pdf'),plot=p,device=cairo_pdf)

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

message(glue('Script complete in {diffmin(t0)} minutes'))