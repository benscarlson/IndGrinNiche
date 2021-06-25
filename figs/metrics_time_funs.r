plotSpec <- function(pop,inddat,aesdat) { #,cidat
  #inddat <- inddf$nind[[1]]
  #pop <- inddf$population[[1]]
  inddat$year <- factor(inddat$year)
  setdat <- inddat %>% group_by(year) %>% summarize(spec=mean(spec))
  
  cols <- aesdat %>% select(short_name,color) %>% deframe #make a named vector
  
  ggplot() +
    #geom_violin(data=inddat,mapping=aes(x=year,y=spec),size=0.8) +
    geom_line(data=inddat, alpha=0.8, #linetype='dashed',
              mapping=aes(x=year,y=spec,group=short_name, color=short_name)) +
    geom_line(data=setdat,mapping=aes(x=year,y=spec,group=1),size=1.3) +
    #geom_errorbar(data=cidat, aes(x=factor(year), ymin=ci_low, ymax=ci_high), width=.1) + #,position=position_dodge(0.05)
    scale_y_continuous(limits=c(0,1)) +
    scale_color_manual(values=cols) +
    guides(color='none') +
    ggtitle(str_to_title(pop))
}

plotNested <- function(pop,pwdat) { #,cidat
  #pwdat <- pwdf$npw[[1]]
  
  pwdat$year <- factor(pwdat$year)
  #setdat$year <- factor(setdat$year)
  #mean of nestedness, per year
  setdat <- pwdat %>% group_by(year) %>% summarize(nestedness=mean(nestedness))
  
  ggplot() +
    #geom_violin(data=pwdat,mapping=aes(x=year,y=nestedness),size=0.8) +
    geom_line(data=pwdat, alpha=0.6, #linetype='dashed',
              mapping=aes(x=year,y=nestedness,group=pair, color=pair)) +
    geom_line(data=setdat,mapping=aes(x=year,y=nestedness,group=1),size=1.3) +
    #geom_errorbar(data=cidat, aes(x=factor(year), ymin=ci_low, ymax=ci_high), width=.1) +
    scale_y_continuous(limits=c(0,1)) +
    scale_color_grey() +
    guides(color='none')
}

plotClust <- function(pop,cldat) {
  cldat$year <- factor(cldat$year)
  
  ggplot() +
    geom_line(data=cldat,mapping=aes(x=year,y=clust_w,group=1),size=1.3) +
    scale_y_continuous(limits=c(0.75,1),breaks=c(0.75,1)) + #
    guides(color='none')
}
