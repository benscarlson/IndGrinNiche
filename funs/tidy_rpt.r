#Start of a broom/tidy function for rpt object
#Only tested with simple, one-group scenario!
#This is still pretty hacky. Need to use tibble more intelligently
#x is a rpt object from rptR package
tidy.rpt <- function(x) {
  r <- t(x$R) %>% as_tibble(rownames = 'group')
  colnames(r) <- c('group','R')
  
  ci <- x$CI_emp
  colnames(ci) <- c('conf.low','conf.high') 
  
  d <- ci %>% as_tibble(rownames = 'group') %>%
    inner_join(r,by='group') %>%
    select(group,R,conf.low,conf.high)
  
  return(d)
}