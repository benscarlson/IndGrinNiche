plotMap <- function(tag,pop,polys,cen,zoom,aesdat) { #tag,
  box <- st_bbox(polys)
  
  #estimate zoom
  # zoom <- make_bbox(
  #   box[c('xmin','xmax')],
  #   box[c('ymin','ymax')]) %>%
  #   calc_zoom + 1
  # print(zoom)
  
  lvs <- aesdat %>% arrange(desc(indiv_vol)) %>% pluck('niche_name')
  
  cols <- aesdat %>% select(niche_name,color) %>% deframe #make a named vector
  
  mp <- getMapRetry(cen,zoom=zoom,'satellite')
  
  #polys$niche_name <- as.character(polys$niche_name)
  
  bb <- attr(mp,'bb')
  
  bbdf <- data.frame(long=c(bb$ll.lon,bb$ur.lon),lat=c(bb$ll.lat,bb$ur.lat))
  
  xmin <- bb$ll.lon + (bb$ur.lon-bb$ll.lon)/15
  xmax <- bb$ur.lon - (bb$ur.lon-bb$ll.lon)/15
  ymin <- bb$ll.lat + (bb$ur.lat-bb$ll.lat)/10
  ymax <- bb$ur.lat - (bb$ur.lat-bb$ll.lat)/10
  
  polys <- polys  %>% mutate(niche_name=factor(niche_name,levels=lvs))
  
  p <- ggmap(mp) +
    geom_sf(data=polys,mapping=aes(color=niche_name),
            inherit.aes=FALSE,fill=NA,show.legend=FALSE,size=0.7)  +
    scale_color_manual(values=cols) +
    ggsn::scalebar(data=NULL,
                   dist = 10, dist_unit='km',transform = TRUE, model = 'WGS84', location='bottomleft',
                   height=0.03, st.size=3, st.dist=0.05, st.color='white',
                   x.min=xmin, x.max=xmax, y.min=ymin, y.max=ymax) +
    labs(
      title=str_to_sentence(pop),
      x='Longitude',
      y='Latitude') +
    theme(
      axis.text=element_text(size=8))
  
  return(p)  
}

plotMDS <- function(gdat,aesdat) {
  #print(tag)
  # pop <- 'beuster-2013'
  # gdat <- mdsgg$data[[1]]
  # aesdat <- mdsgg$aesdat[[1]]

  lvs <- aesdat %>% arrange(desc(indiv_vol)) %>% pluck('niche_name')
  
  cols <- aesdat %>% select(niche_name,color) %>% deframe #make a named vector
  #tl <- ifelse(is.na(tag),str_to_title(pop),glue('{tag} {str_to_title(pop)}'))
  
  gdat %>%
    mutate(niche_name=factor(niche_name,levels=lvs)) %>%
  ggplot(aes(MDS1, MDS2, color=niche_name)) + #
    geom_convexhull(fill=NA,show.legend=FALSE,size=1.2) +
    #scale_color_brewer(palette='Paired') +
    scale_color_manual(values=cols) +
    theme(
      axis.text=element_blank(),
      axis.ticks=element_blank())
  
  # ,
  # axis.title.y=element_text(vjust=0.5)
}

plotViolin <- function(pop,gdat,nsdat) {
  #yellowish: F3E939, blueish: 6975A6, orangeish: F28A30
  
  ggplot() +
    geom_violin(data=gdat,mapping=aes(x=metric,y=value), #, fill=metric)
                width=0.4,size=0.6,alpha=0.6,color='grey') + #
    geom_point(data=nsdat,mapping=aes(x=metric,y=value,fill=metric), #, fill=metric
               size=4, shape=21, stroke=0.8,show.legend=FALSE) +
    scale_y_continuous(limits=c(0,1)) +
    coord_flip() + 
    guides(fill=FALSE) +
    scale_fill_manual(values=c('#F28A30','#6975A6','#F3E939')) +
    #scale_fill_manual(values=c('red','blue')) +
    theme(
      axis.title=element_blank(),
      axis.text.y = element_text(angle = 90, hjust=0.5, size=9,color='black')) #
}
