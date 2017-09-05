#'
#' Creates a time-series of the traded volumes for a given time period.
#'
#'@param data MDR or TRS data.
#'@param contract The contract that you are pulling data for.
#'@param lookBackPeriod A list containing the lookback units and size for a rolling volume calculation specified in secs,hours,day,weeks,months
#'
#'@author Helen Ristov and Cameron Sawyer
#'
#'@export
#'
#'@import data.table
#'

Calc.Volumes <- function(data, contract, lookBackPeriod = 1) {
  # drop all data but volume
  volume <- data$TradedVolume
  
  # normalize to one second bars
  volume <- align.time(volume, 1)
  volume <- data.table(index = as.POSIXct(index(volume)), TradedVolume = as.numeric(volume[,'TradedVolume']))
  volume <- volume[, list(volume = sum(TradedVolume, na.rm = TRUE)), by = "index"]
  volume <- xts(volume$volume, order.by = volume$index)
  SquareTime <- seq(start(volume), end(volume), 1)
  volume <- na.fill(merge(volume, SquareTime), 0)
  
  # determine rolling volume over lookback period
  if(length(lookBackPeriod) != 1){
    ep     <- endpoints(volume, on = lookBackPeriod$units, k = lookBackPeriod$size)
    volume <- period.apply(volume, ep, FUN=sum)
    colnames(volume)   <- c(paste0(contract, ".Volume.", lookBackPeriod$size, lookBackPeriod$units))
  }else{
    colnames(volume)   <- c(paste0(contract, ".Volume.1sec"))
  }
  
  return(volume) 
}