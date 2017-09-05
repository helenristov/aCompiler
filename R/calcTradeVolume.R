#'
#' Creates a time-series of the traded volumes for a given time period.
#'
#'@param data MDR or TRS data.
#'@param contract The contract that you are pulling data for.
#'@param lookBackPeriod A list containing the lookback units and size for a rolling volume calculation specified in secs,hours,day,weeks,months
#'
#'@author Helen Ristov 
#'
#'@export
#'
#'@import data.table
#'

calcTradeVolume <- function(data, contract, lookBackPeriod = 1) {

  # normalize to one second bars
  data <- align.time(data, 1)
  ##  identify bid and ask volume 
  data$BidVolume <-  ifelse(!is.na(data$TradedVolume) & data$TradedPrice == data$BestBid, data$TradedVolume, 0)
  
  data$AskVolume <-  ifelse(!is.na(data$TradedVolume) & data$TradedPrice == data$BestAsk, data$TradedVolume, 0)

  combined <- data.table(index = as.POSIXct(index(data)), 
                         TradedVolume = as.numeric(data[,'TradedVolume']), 
                         BidVolume = as.numeric(data[,"BidVolume"]),
                         AskVolume = as.numeric(data[,"AskVolume"]))
  
  combined <- combined[, list(volume = sum(TradedVolume, na.rm = TRUE), bidvolume = sum(BidVolume), askvolume = sum(AskVolume)), by = "index"]
  SquareTime   <- seq(combined$index[1], combined$index[nrow(combined)], 1)
  final <- merge(as.xts(combined), SquareTime)
  final$volume[which(is.na(final$volume))] <- 0
  final$bidvolume[which(is.na(final$bidvolume))] <- 0
  final$askvolume[which(is.na(final$askvolume))] <- 0
  
  
  # determine rolling volumes over lookback period
  if(length(lookBackPeriod) != 1){
    ep     <- endpoints(final, on = lookBackPeriod$units, k = lookBackPeriod$size)
    volume    <- period.apply(final$volume, ep, FUN=sum)
    bidvolume <- period.apply(final$bidvolume, ep, FUN=sum)
    askvolume <- period.apply(final$askvolume, ep, FUN=sum)
    final    <- merge(volume,bidvolume,askvolume)
    index(final) <- index(final)+1
    
  }
  
  return(final) 
}