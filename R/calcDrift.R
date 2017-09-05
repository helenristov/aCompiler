#' Calculates a drift measure for a price series
#'
#'@param spread A price series on which you want to compute the drift
#'@param dPeriod The number of days used to compute the drift
#'@param .combine A boolean that sets if we want to merge the periods and return the mean of the rolling drift measures over those periods. 
#'@author Helena Ristov
#'
#'@export

calcDrift  <- function(spread, dPeriod, .combine = TRUE){
  
  
  incr              <- abs(as.numeric(difftime(index(spread[1]), index(spread[2]), units = "secs")))
  IncrPerDay        <- length(which(trunc.POSIXt(index(spread), units = "days") == trunc.POSIXt(index(spread), units = "days")[1]))
  spreadName        <- colnames(spread)
  spread$Period.SMA <- SMA(spread, n = IncrPerDay*dPeriod)
  spread$SMA.Diff   <- diff(spread$Period.SMA)
  
  nas   <- spread$Period.SMA[which(is.na(spread$Period.SMA))]
  fill  <- spread$Period.SMA[-which(is.na(spread$Period.SMA))]
  
  SMA.max   <- cummax(fill)
  SMA.min   <- cummin(fill)
  SMA.range <- SMA.max -  SMA.min
  
  spread$Period.SMA.Max   <- rbind(nas, SMA.max)
  spread$Period.SMA.Min   <- rbind(nas, SMA.min)
  spread$Period.SMA.Range <- spread$Period.SMA.Max - spread$Period.SMA.Min
  
  drift                  <- spread

  if(.combine == TRUE){
    drift                <- mean(spread$SMA.Diff, na.rm = TRUE)
  }
  
  return(drift)
}

