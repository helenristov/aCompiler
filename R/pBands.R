#' Calculates the pband levels on a spread price series
#'
#'@param spread A price series on which you want to compute the probability bands.
#'@param dFast The number of days used to compute the fast moving average
#'@param dSlow The number of days used to compute the slow moving average
#'@param minFact The number of standard deviations to set the lower bound of the scalping bounds
#'@param maxFact The number of standard deviations to set the upper bound of the scalping bounds
#'@author Helena Ristov
#'
#'@export

pBands  <- function(spread, dFast, dSlow, minFact, maxFact, incr = 0, minVol = 0, FUN = NULL, p.output = FALSE){
  
  ##period is specified in seconds so determine
  if(incr == 0){ incr <- abs(as.numeric(difftime(index(spread[1]), index(spread[2]), units = "secs"))) }
  IncrPerDay <- IncrPerWindow(as.character(first(index(spread))), as.character(last(index(spread))), incr = incr, window.size = 1, window.size.units = "days")
  
  nSlow <- floor(max(1, round(IncrPerDay * dSlow)))
  nFast <- floor(max(1, round(IncrPerDay * dFast)))
  
  if(is.null(FUN)){ FUN <- 'SMA' }
  if(!FUN %in% c('SMA','EMA')){ stop('pBands function only works with SMA or EMA functions.') }
  
  ## nFast and nSlow correspond to the number of periods in the slow versus fast moving average.
  spread$Fast   <- do.call(FUN, c(list(spread)    , n = nFast))
  spread$Slow   <- do.call(FUN, c(list(spread[,1]), n = nSlow))
  spread$Diff   <- spread$Slow - spread$Fast
  
  NAs      <- which(is.na(apply(spread$Diff, 1, sum)))
  MA.Diff  <- spread$Diff[-NAs,]
  
  ## Determine how to calculate MA vol diff
  if(FUN == 'SMA'){
    sdev   <- pmax(as.xts(runSD(MA.Diff, n = nSlow)), minVol)
  }else{
    MA.Diff$Mean  <- EMA(MA.Diff, n = nSlow)
    sdev <- pmax(sqrt(EMA((MA.Diff$Mean - MA.Diff$Diff)^2, n = nSlow)), minVol)
    colnames(sdev) <- "sdev"
  }
  
  spread <- merge(spread, sdev, all.x = TRUE)
  spread <- spread[,-ncol(spread)]
  
  spread$PB.UB.A  <- spread$Slow + maxFact*spread$sdev
  spread$PB.LB.A  <- spread$Slow + minFact*spread$sdev
  spread$PB.UB.B  <- spread$Slow - maxFact*spread$sdev
  spread$PB.LB.B  <- spread$Slow - minFact*spread$sdev
  
  if(p.output){
    spread$MeanDiff <- MA.Diff$Mean  
    output = list(spread = spread, lambdas = list(Slow = 2/(nSlow + 1), Fast = 2/(nFast + 1)))
    return(output)
  }
  
  return(spread)
}


