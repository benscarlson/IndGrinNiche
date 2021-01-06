#'Sets up individual panels with common axis labels
#'Right now just works with a row of three panels
rowPanel <- function(ggl,xlab=NULL,ylab=NULL,xhide=FALSE) {
  
  if(xhide) {
    ggl <- ggl %>% map(~{
      . + theme(axis.text.x=element_blank(),
                axis.ticks.x=element_blank())
    })
  }
  
  ggl[[1]] <- ggl[[1]] + 
    theme(axis.title.x=element_blank())
  
  if(!is.null(ylab)) { ggl[[1]] <- ggl[[1]] + labs(y=ylab)}
  
  ggl[[2]] <- ggl[[2]]  +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  if(!is.null(xlab)) { 
    ggl[[2]] <- ggl[[2]] + labs(x=xlab) + theme(axis.title.y=element_blank())
  } else {
    ggl[[2]] <- ggl[[2]] + theme(axis.title=element_blank())
  }
  
  ggl[[3]] <- ggl[[3]]  +
    theme(axis.title=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  return(ggl)
}