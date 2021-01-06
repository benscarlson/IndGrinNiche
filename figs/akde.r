#!/usr/bin/env Rscript

#TODO: need a more general way to do zoom. Right now, zoom is optimized for year 2015.
'
Figure of home ranges

Usage:
2_akde <hr> <year> <out> [--seed=<seed>]
2_akde (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
' -> doc

#---- Input Parameters ----#
if(interactive()) {
  library(here)
  
  .wd <- '~/projects/whitestork/results/stpp_models/huj_eobs'
  .script <- 'src/script_template.r' #Currently executing script
  .seed <- NULL
  rd <- here
  
  .hrP <- file.path(.wd,'ctmm/akde_contour/contours.shp')
  .year <- 2015
  .outPF <- file.path(.wd,'figs/ms/2',glue('2_akde_{.year}.pdf'))

} else {
  library(docopt)
  library(rprojroot)
  
  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .hrP <- ag$hr
  .year <- ag$year
  .outPF <- ag$out
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
spsm(library(sf))
spsm(library(uuid))

source(rd('src/funs/funs.r'))
source(rd('src/funs/themes.r'))
theme_set(theme_ms)

#---- Parameters ----#
.runid <- UUIDgenerate()
.parPF <- file.path(.wd,"run_params.csv")

#---- load data ----#
nsets <- read_csv(file.path(.wd,'niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set') %>%
  filter(year==.year)

polys0 <- st_read(.hrP,quiet=TRUE) %>%
  filter(est=='est') %>%
  select(niche_name=n_name) %>% #have to rename b/c shapefile
  inner_join(niches %>% select(niche_name,population,individual_id),by='niche_name')

plotMap <- function(tag,pop,polys,cen,zoom) { #tag,
  box <- st_bbox(polys)
  
  #estimate zoom
  # zoom <- make_bbox(
  #   box[c('xmin','xmax')],
  #   box[c('ymin','ymax')]) %>%
  #   calc_zoom + 1
  # print(zoom)
  
  mp <- getMapRetry(cen,zoom=zoom,'satellite')
  
  #polys$niche_name <- as.character(polys$niche_name)
  
  bb <- attr(mp,'bb')
  
  bbdf <- data.frame(long=c(bb$ll.lon,bb$ur.lon),lat=c(bb$ll.lat,bb$ur.lat))
  
  xmin <- bb$ll.lon + (bb$ur.lon-bb$ll.lon)/15
  xmax <- bb$ur.lon - (bb$ur.lon-bb$ll.lon)/15
  ymin <- bb$ll.lat + (bb$ur.lat-bb$ll.lat)/10
  ymax <- bb$ur.lat - (bb$ur.lat-bb$ll.lat)/10
  
  p <- ggmap(mp) +
    geom_sf(data=polys,mapping=aes(color=niche_name),
            inherit.aes=FALSE,fill=NA,show.legend=FALSE)  +
    ggsn::scalebar(data=NULL,
                   dist = 10, dist_unit='km',transform = TRUE, model = 'WGS84', location='bottomleft',
                   height=0.03, st.size=3, st.dist=0.05, st.color='white',
                   x.min=xmin, x.max=xmax, y.min=ymin, y.max=ymax) +
    labs(
      title=glue('{tag} {str_to_sentence(pop)}'),
      x='Longitude',
      y='Latitude') +
    theme(
      axis.text=element_text(size=8))
  
  return(p)  
}

polydf <- polys0 %>%
  nest(data=-population) %>%
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
    zoom=c(11,10,10), #manually put in zoom
    tag=letters[row_number()]
  ) %>%
  mutate(gg=pmap(list(tag,population,data,cen,zoom),plotMap)) #tag)
#polydf$gg[[1]]

#formatting specific to each plot
polydf$gg[[1]] <- polydf$gg[[1]] + theme(axis.title.x=element_blank())

polydf$gg[[2]] <- polydf$gg[[2]] + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

polydf$gg[[3]] <- polydf$gg[[3]] + 
  theme(axis.title=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

plist <- polydf$gg

p <- wrap_plots(plist,nrow=1,ncol=3)  #+
  # plot_annotation(
  #   title=glue('{.year}'),
  #   #tag_levels='A'
  # );

h=4; w=10
if(fext(.outPF)=='pdf') {
  ggsave(.outPF,plot=p,height=h,width=w,device=cairo_pdf) #save pdf
} else if(fext(.outPF)=='png') {
  ggsave(.outPF,plot=p,height=h,width=w,type='cairo')
}

#ggsave(.outPF,plot=p,height=4,width=10,type='cairo')

#---- Finalize script ----#
#Save all parameters to csv for reproducibility
#TODO: write this to a workflow database instead
saveParams(.parPF)

message(glue('Script complete in {diffmin(t0)} minutes'))
