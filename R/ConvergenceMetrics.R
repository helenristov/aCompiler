
#' Convergence test based on a disjoint distance from the limit value
#'
#'@param data xts series to be tested for convergence (must be nx1)
#'@param limit value at which the series converges to
#'@param epsilon distance from limit which sets the convergence zone
#'@param threshold value above and below the limit which determine if an observation is disjoint
#'@param perf.period number of bars to test for convergence
#'
#'@return disjoint the distance the current observation is above or below the current threshold
#'@return convBool boolean to indicate if the series converged at some point during the performance period 
#'
#'@author Helena Ristov
#'
#'@export


convergenceMetrics <- function(data, limit, epsilon, threshold, perf.period) {

  ##period is specified in seconds so determine
  
  incr              <- abs(as.numeric(difftime(index(data), index(data), units = "secs")))
  IncrPerDay        <- length(which(trunc.POSIXt(index(data), units = "days") == trunc.POSIXt(index(data), units = "days")[1]))
  
  highs <- rollmaxr(data,  IncrPerDay*perf.period)
  lows  <- rollmaxr(-data, IncrPerDay*perf.period)
  
  highs <- xts(highs, order.by=index(data[1:length(highs)]))
  lows  <- xts(lows, order.by=index(data[1:length(lows)]))
  
  filler  <- xts(rep(NaN, length(data)- length(highs)), order.by = index(data[c((length(highs)+1):length(data)),]))
  highs   <- rbind(highs, filler)
  lows    <- rbind(lows, filler)
  

  disjoint <- ifelse(data > limit + threshold, 
                     data - (limit + threshold), 
                     ifelse(data < limit - threshold, data - (limit - threshold), 0))
  
  c1      <- ifelse(disjoint > 0, lows <= limit + threshold,
                     ifelse(disjoint < 0, highs >= limit - threshold, NA))
  
  conv        <- cbind(data, disjoint, c1, highs, lows)
  names(conv) <- c(colnames(data), "disjoint","convBool", "perfHigh", "perfLow")
  
  return(conv)
}