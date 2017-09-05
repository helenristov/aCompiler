#' Calculates the Aroon oscillator and outputs a signal for buy and sell events
#'
#'@param spread.data A price series 
#'@param vol.settings A list with the vol settings to be used for the threshold computation
#'@param volBand The number of standard deviations that set the zone around which we compute the aroon values. 
#'@param aroon.settings The time bucket for the Aroon oscillator and the threshold value for the buy and sell signal  
#'@param use.volFilter filter out any values that are inside the volBand
#'
#'@return list of Aroon indicators and triggers:
#' \item{aroon.up.sig}{The value of the oscillator if the threshold on the upside is breached and outside the volBand, NA otherwise}
#' \item{aroon.down.sig}{The value of the oscillator if the threshold on the downside is breached and outside the volBand, NA otherwise}
#' \item{binary.signal}{The combinations of the up and down signal into one field with 0 implying no action, -1 implying a sell, and 1 implying a buy}
#' \item{binary.filter}{The binary signal with consecutive buy and sell signals filtered out}
#'@author Helena Ristov and Cameron Sawyer
#'
#'@export
#'
#'
Aroon <- function(spread.data, vol.settings, volBand, aroon.settings, use.volFilter = TRUE, ...) {
  
  ##generate pBands volatility -- currently only support for pBands
  if(vol.settings$type == 'PBands'){
    
    pBands.measures <- calcVol( spread.data, 
                                vol.settings,
                                type = 'PBands', 
                                .combine = FALSE,
                                ...)
    vol <- pBands.measures$Vol
    FV  <- pBands.measures$SMA.Slow
  }
  
  UB <- FV + volBand * vol
  LB <- FV - volBand * vol
  
  # count <- c(1,2)
  # len <- nrow(spread.data)
  # 
  # aroonup <- numeric(len)
  # aroondown <- numeric(len)
  # 
  # ##compute aroon up and down
  # for (i in aroon.settings$time.bucket : len) {
  #   aroonup[i]   <- (aroon.settings$time.bucket - (aroon.settings$time.bucket - which.max(spread.data[(i + 1 - aroon.settings$time.bucket) : i]))) / aroon.settings$time.bucket
  #   aroondown[i] <- (aroon.settings$time.bucket - (aroon.settings$time.bucket - which.min(spread.data[(i + 1 - aroon.settings$time.bucket) : i]))) / aroon.settings$time.bucket
  # }
  
  aroonUp <- .Call("aroon_max",  spread.data, aroon.settings$time.bucket, PACKAGE = "TTR")/100
  aroonDn <- .Call("aroon_max", -spread.data, aroon.settings$time.bucket, PACKAGE = "TTR" )/100
  
  # run Aroon analysis
  Aroon.Data <- cbind(spread.data, FV, UB, LB, aroonUp, aroonDn)
  colnames(Aroon.Data) <- c('spread', 'FV', 'UB', 'LB', 'aroonup', 'aroondown')
  
  # compute trend indicators
  Aroon.Data$oscillator <- Aroon.Data$aroonup - Aroon.Data$aroondown
  
  ## Find the aroon signal
  Aroon.Data$aroon.up.sig   <- ifelse(sign(Aroon.Data$oscillator) ==  1 & Aroon.Data$oscillator >  aroon.settings$signal.strength, Aroon.Data$oscillator, NA)
  Aroon.Data$aroon.down.sig <- ifelse(sign(Aroon.Data$oscillator) == -1 & Aroon.Data$oscillator < -aroon.settings$signal.strength, Aroon.Data$oscillator, NA)
  
  ## Filter out any aroon signals that are inside the vol band
  if(use.volFilter == TRUE){
    index <- c(1:nrow(Aroon.Data))
    index <- index[-which(Aroon.Data$spread >= Aroon.Data$UB | Aroon.Data$spread <= Aroon.Data$LB)]   ## find the values that we want to consider outside of indeterminate zone
    Aroon.Data$aroon.up.sig[index]   <- NA
    Aroon.Data$aroon.down.sig[index] <- NA
  }
  
  # combine aroontrend up and aroon trend down signals into single binary_signal vector
  Aroon.Data$binary.signal <- 0
  Aroon.Data$binary.signal[which(!is.na(Aroon.Data$aroon.up.sig))]   <-  1
  Aroon.Data$binary.signal[which(!is.na(Aroon.Data$aroon.down.sig))] <- -1
  
  ##remove consecutive triggers
  binary.filter <- ifelse(as.numeric(Aroon.Data$binary.signal)[1:(nrow(Aroon.Data)-1)] == as.numeric(Aroon.Data$binary.signal)[2:nrow(Aroon.Data)], 0, Aroon.Data$binary.signal[2:nrow(Aroon.Data)])
  Aroon.Data$binary.filter <- c(binary.filter, 0)
  
  Aroon.Data <- Aroon.Data[,c('spread', 'aroonup', 'aroondown', 'oscillator', 'aroon.up.sig', 'aroon.down.sig', 'binary.signal', 'binary.filter')]
  return(Aroon.Data)
  
}