#'
#' Creates the indicator for a Spread Scalping Strategy
#' 
#' Generates a fixed price scalp indicator for a given data set.  This inidcator is typically used to scalp around a stationary price series.
#'
#'@param Data Single price series to generate a scalping FV indicator.
#'@param Vol The volatility measure for the given data above
#'@param InitVolFactor The multiplier of the Vol that determines the initial entry price.
#'@param MaxVolFactor The multiplier of the Vol that determines the max position price.
#'@param AdjFactor The amount that a price should be adjusted when the price exceeds the max price limit.
#'
#'@author Nicholas Dregne
#'

ScalpFVInd <- function(Data, Vol, InitVolFactor, MaxVolFactor, AdjFactor){
  
  Incr <- as.numeric(InitVolFactor * Vol)
  MaxI <- as.numeric(MaxVolFactor  * Vol)
  AdjF <- as.numeric(AdjFactor) / MaxI
  
  FV  <- xts(rep(Data[1,], nrow(Data)), order.by = index(Data))
  FVN <- (FV   - as.numeric(FV[1,])) / MaxI
  WPN <- (Data - as.numeric(FV[1,])) / MaxI
  NotDone <- TRUE
  
  while(NotDone){
    Breaches <- which(abs(WPN - FVN) > 1)
    
    if(!(length(Breaches) > 0)){
      NotDone <- FALSE
      rm(Breaches)
      next
    }
    
    if(exists('FirstBreach')){ FirstBreach <- append(FirstBreach, first(Breaches)) }else{ FirstBreach <- first(Breaches) }
    
    UpdateRow <- last(FirstBreach)
    if(UpdateRow < nrow(FVN)){ 
      FVN[UpdateRow:nrow(FVN),] <- FVN[UpdateRow:nrow(FVN),] + as.numeric(sign(WPN[UpdateRow,] - FVN[UpdateRow,])) * AdjF
    }
    
    rm(list = c('Breaches', 'UpdateRow'))
  }
  
  if(exists("FirstBreach")){
    FVN[FirstBreach,] <- FVN[FirstBreach,] - as.numeric(sign(WPN[FirstBreach,] - FVN[FirstBreach,])) * AdjF
    rm(FirstBreach)  
  }
  
  NewFV <- FVN * MaxI + FV[,1]
  Bands <- xts(matrix(nrow = nrow(NewFV), ncol = 5), order.by = index(NewFV))
  colnames(Bands) <- c('MaxBid','InitBid','FV','InitAsk','MaxAsk')
  
  Bands[,'FV'     ] <- NewFV
  Bands[,'MaxBid' ] <- NewFV - MaxI
  Bands[,'InitBid'] <- NewFV - Incr
  Bands[,'InitAsk'] <- NewFV + Incr
  Bands[,'MaxAsk' ] <- NewFV + MaxI
  
  return(Bands)
}
