outliers <- function(mv,maxSpeed) {
  
  outlier <- rep(FALSE,nrow(mv))
  speed <- c(NA,move::speed(mv)) #looping over vector is much faster than over mv object
  
  i <- 2 #start with 2 because first row has NA
  while(i <= nrow(mv)) {
    
    if(speed[i] > maxSpeed) {
      outlier[i] <- TRUE
      i <- i + 1 #if outlier, we need to skip the next point, since speed will be high on the "return"
    }
    i <- i + 1
  }
  
  return(outlier)
}