#'
#' Creates a VWAP series for a given contract
#'
#'@param data MDR or TRS data.
#'@param contract The contract that you are pulling data for.
#'@param lookBackPeriod The lookback period of rolling VWAP calculation in seconds.
#'
#'@author Helen Ristov and Cameron Sawyer
#'
#'@export
#'
#'@import data.table
#'

Calc.VWAP <- function(data, contract, lookBackPeriod){
  require(data.table)
  # drop all data but volume and price
  
  volume <- data[, c("TradedVolume", "TradedPrice")]
  
  # normalize to one second bars
  volume <- align.time(volume, 1) 
  volume <- data.table(index = as.POSIXct(index(volume)), TradedVolume = as.numeric(volume[,'TradedVolume']), TradedPrice = as.numeric(volume[,'TradedPrice']))
  volume <- volume[, list(vwap = sum(TradedVolume * TradedPrice, na.rm=TRUE) / sum(TradedVolume, na.rm = TRUE), volume = sum(TradedVolume, na.rm = TRUE)), by = "index"] 
  
  volume <- xts(cbind(volume$vwap, volume$volume), order.by = as.POSIXct(volume$index)) 
  colnames(volume) <- c('vwap', 'volume') 
  SquareTime <- seq(start(volume), end(volume), 1)
  volume <- merge(volume, SquareTime) 
  
  func.vwap <- function(volume) {
    vwap <- sum(volume[, 'vwap'] * volume[, 'volume'], na.rm = TRUE) / sum(volume[, 'volume'], na.rm = TRUE)
    return(vwap)
  }
  
  if(lookBackPeriod == 1){
    vwap <- volume
  }else{
    vwap <- rollapply(volume, 
                      lookBackPeriod, 
                      func.vwap, 
                      by.column = FALSE)
  }
  ## vwap <- na.locf(vwap)
  
  vwap <- vwap$vwap
  
  return(vwap)
}
