#' Calculates a low amplitude sinusoidal regression based on a spectral analysis 
#'
#'@param data A price series 
#'@param window The window of time for the spectral fit which can be rolling or anchored
#'@param incr The increment of time that the prices series is squared at 
#'@param anchor.window The anchor window that will be used to start the cumulative fit   
#'
#'@return list of values are returned
#' \item{lastValue}{The last fitted FV}
#' \item{fitted.values}{The entire range of fitted fair values }
#'@author Helena Ristov and Nicholas Dregne
#'
#'@export
#'

Calc.Sinusoidal.FV <- function(data, window, incr, anchor.window = FALSE){
  
  incr.p.window <- IncrPerWindow(as.character(first(index(data))), as.character(last(index(data))), incr, window$n, window$units)
  
  if(anchor.window){
    ##Generate the values on the anchor window
    anchor.fit.values <- as.xts(sinusoidalFit(data[1:incr.p.window])$fitted.values)
    
    ##compute the incremental values off the anchor
    num <- nrow(data) - incr.p.window
    incremental.values <- sapply(1:num, function(i){sinusoidalFit(data[1:(num+i)])$lastValue})
    incremental.values <- as.xts(incremental.values, order.by = index(data)[num+1:nrow(data)])
    
    output <- rbind(anchor.fit.values, incremental.values)
  }else{
    output <- rollapply(data, width = incr.p.window, function(x){sinusoidalFit(x)$lastValue})
  }
  return(output)
}
