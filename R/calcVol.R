#' Calculates the volatility of a spread
#'
#'@param spread A price series on which you want to compute the hourly volatility measure over a specified period of time
#'@param dPeriod The number of days used to compute the hourly volatility. Default is set to NA and should be used when selecting PBands type
#'@param dFast  The number of days for the pbands fast moving average
#'@param dSlow The number of days for the pbands slow moving average
#'@param type The type of volatility to compute. Valid selections are Diff and PBands
#'@param .combine A boolean that sets if we want to merge the periods and return the mean of the rolling vol measures over those periods.
#'@author Helena Ristov
#'
#'@export


calcVol <- function(spread, vol.settings, type = 'Diff', .combine = TRUE, incr = 0, minVol = NULL){
  
  ##period is specified in seconds so determine
  if(incr == 0){ incr <- abs(as.numeric(difftime(index(spread[1]), index(spread[2]), units = "secs"))) }
  
  start.date <- as.character(index(spread)[1])
  end.date   <- as.character(index(spread)[length(spread)])
  
  n.slow <- IncrPerWindow(start.date, end.date, incr, window.size = vol.settings$dSlow.size, window.size.units = vol.settings$dSlow.units)
  n.fast <- IncrPerWindow(start.date, end.date, incr, window.size = vol.settings$dFast.size, window.size.units = vol.settings$dFast.units)
  if(toupper(type) == 'DIFF' & .combine == TRUE){
    Vol        <-  sd(diff(spread), na.rm = TRUE) * sqrt(3600 / incr)    ##hourly vol across entire spread
  }else if(toupper(type) == 'DIFF'){
    spread$diff.vol <- runSD(diff(spread), n = n.slow, cumulative = FALSE) * sqrt(3600 / incr) 
    Vol             <- spread
  }else if(toupper(type) == 'PBANDS'){
    spreadName        <- colnames(spread)
    spread$SMA.Fast   <- SMA(spread[,1], n = n.fast)
    spread$SMA.Slow   <- SMA(spread[,1], n = n.slow)
    spread$SMA.Diff   <- spread$SMA.Slow - spread$SMA.Fast
    NAs               <- which(is.na(apply(spread$SMA.Diff, 1, sum)))
    SMA.Diff          <- spread$SMA.Diff[-NAs,]
    spread$Vol        <- runSD(SMA.Diff, n = n.slow, cumulative = FALSE)
    
    Vol               <- spread
    colnames(Vol)     <- c(spreadName, "SMA.Fast", "SMA.Slow", "SMA.Diff", "Vol")
    if(.combine == TRUE){
      Vol             <- mean(spread$Vol, na.rm = TRUE)  
    }
  }
  
  if(!is.null(minVol)){ if(.combine){ Vol <- max(Vol, minVol) }else{ Vol <- pmax(Vol, minVol) } }
  
  return(Vol)
  
}  
