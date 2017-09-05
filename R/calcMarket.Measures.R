#' Calculates Standard Market Micro-Structures (WMP, MP, Bid-Ask)
#'
#'@param market.data Raw TOB market data from either TRS or MDR
#'@param contract Contract for which you desire to get market micro-structure measures.
#'@param TT_MinTick A boolean to determine if we should use the TT mintick or TRS mintick.
#'
#'@author Nicholas Dregne
#'
#'@export
#'

## Measure used to calc WMP
calcMarket.Measures <- function(market.data, contract, TT_MinTick = FALSE){
  
  mt <- getContractMT(contract, TT_MinTick = TT_MinTick)
  
  if(substr(contract, 1, 2) %in% c('GE','ED')){
    HTDate <- as.POSIXlt(paste0(GEHalfTick(contract, as.Date(index(market.data)[1])) - 1, " ", getInfo(contract)$times$a.open))
    HTRows <- which(as.POSIXlt(index(market.data)) >= HTDate)
    
    if(length(HTRows) > 0){
      mt <- rep(mt, nrow(market.data))
      mt[HTRows] <- mt[1] / 2 
    }
  }
  
  ## calculate bid ask spread 
  market.data$BA.Spread <- market.data$BestAsk - market.data$BestBid
  
  ## calculate mid point
  market.data$MP <- (market.data$BestAsk + market.data$BestBid)/2
  
  ## calculate WMP 
  market.data$WMP <- ifelse(market.data$BA.Spread > mt * 1.01,
                           (market.data$BestAsk + market.data$BestBid)/2,
                           (market.data$BestBid*market.data$AskQty + market.data$BestAsk*market.data$BidQty)/(market.data$BidQty+market.data$AskQty))
  
  return(market.data)
}