#' Calculates the range of a spread computed by percentile bands to minimize the impact of outliers
#'
#'@param spread A price series on which you want to compute the range of a spread
#'@param dPeriod The number of days for the period. The default is set to NA with .combine = true to compute the range over the entire spread
#'@param minProb The percentile to set the lower band of the range calculation
#'@param maxProb The percentile to set the upper band of the range calculation 
#'@param .combine A boolean that sets if want to compute the range over the entire spread 
#'@author Helena Ristov
#'
#'@export

calcRange <- function(spread, dPeriod = NA, minProb, maxProb, .combine = TRUE){
  
  if(is.na(dPeriod) & .combine == FALSE){
    stop("This is not a valid combination")
  }
  
  ##period is specified in seconds so determine
  incr              <- abs(as.numeric(difftime(index(spread[1]), index(spread[2]), units = "secs")))
  IncrPerDay        <- length(which(trunc.POSIXt(index(spread), units = "days") == trunc.POSIXt(index(spread), units = "days")[1]))
  
  quantileMax <- function(spread){
    quantile <-  quantile(spread, probs = maxProb, na.rm = FALSE, names = TRUE, type = 7)
    return(quantile)
  }
  
  quantileMin <- function(spread){
    quantile <-  quantile(spread, probs = minProb, na.rm = FALSE, names = TRUE, type = 7)
    return(quantile)
  }
  
  
  if(is.na(dPeriod) & .combine == TRUE){
    
    spread.mean   <- mean(spread)
    spread.min    <- quantile(spread, probs = minProb, na.rm = FALSE, names = TRUE, type = 7)
    spread.max    <- quantile(spread, probs = maxProb, na.rm = FALSE, names = TRUE, type = 7)
    range         <- c(spread.max, spread.min, abs(spread.max - spread.min))
    names(range)  <- c("max", "min", "range")
  } 
  else{
    
    if(!is.na(dPeriod)){
      
      
      spread.max       <-  rollapply(spread, width = IncrPerDay*dPeriod, FUN = quantileMax)
      spread.min       <-  rollapply(spread, width = IncrPerDay*dPeriod, FUN = quantileMin)
      spread.range     <-  abs(spread.max - spread.min)
      range            <-  merge(spread.max, spread.min, spread.range)
      colnames(range)  <- c("max", "min", "range")
    }
    if(.combine){
      range          <- apply(na.trim(range), 2, mean) 
    }
  }
  
  return(range)
}