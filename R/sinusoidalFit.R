#' Fits a sinusoidal curve to a time-series object
#'
#'@param spread.data A price series for fitting.
#'
#'@return list of values are returned
#' \item{lastValue}{The last fitted FV}
#' \item{fitted.values}{The entire range of fitted fair values }
#'@author Helena Ristov and Nicholas Dregne
#'
#'@export
#'

sinusoidalFit <- function(spread.data){
  t    <- 1:nrow(spread.data)
  ssp  <- spectrum(spread.data, plot = FALSE)
  
  per   <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
  reslm <- lm(spread.data ~ sin(2*pi/per*t)+cos(2*pi/per*t)) 
  
  Output <- list(lastValue = last(reslm$fitted.values), fitted.values = reslm$fitted.values)
  
  return(Output)
}
