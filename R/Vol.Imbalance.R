#' Calculates A Volume Imbalance Trigger from Buy and Sell Raw Data
#'
#'@param contract An Instrument identifier 
#'@param contract.type Types of Syn and Direct
#'@param start.date The start time of the evaluation period
#'@param end.date The end time of the evaluation period
#'@param incr  The increment for the volume imbalance rollup. This is a list with units and size parameters
#'
#'@return list of Aroon indicators and triggers:
#' \item{imb.sell.trigger}{A trigger of 1 when there is abnormal sell activity occurring in the market}
#' \item{imb.buy.trigger}{A trigger of 1 when there is abnormal buy activity occurring in the market}

#'@author Helena Ristov
#'
#'@export
#'
#'

Vol.Imbalance <- function(contract, contract.type, start.date, end.date, incr = list(units = "hours", size = 1)){
  
  load(paste0('/data/synthetics/spread.meta.data/', last.trade.date, "_SpreadMetaData.RData"))
  last.trade.date <- GetWeekDays(Sys.Date(), Sys.Date()-7)[2]
  
  if(contract.type == "Syn"){
    
    legs <- c(BetaMetaData[which(Spread == contract)]$Front, BetaMetaData[which(Spread == contract)]$Back)
  
  }else if(contract.type =="Direct"){
    
    legs <- contract
  
  }else{
   
    stop(print("contract type not supported"))
  }
  
  volume.data <- xts()
  for(leg in legs){
    data   <- Pull.TRS.Data(leg, start.date, end.date)[[1]]
    volume <- calcTradeVolume(data, leg, lookBackPeriod = incr)  ##hourly volumes 
    colnames(volume) <- paste0(leg, ".", colnames(volume))
    volume.data <- cbind(volume, volume.data)
  }
  
  ## vol metrics
  if(contract.type == "Syn"){
    volume.data$Total.Volume  <- volume.data[,paste0(legs[1],".volume")]    + volume.data[,paste0(legs[2], ".volume")]
    volume.data$Long.Volume   <- volume.data[,paste0(legs[1],".askvolume")] + volume.data[,paste0(legs[2],".bidvolume")] 
    volume.data$Short.Volume  <- volume.data[,paste0(legs[1],".bidvolume")] + volume.data[,paste0(legs[2],".askvolume")] 
    
  }else if(contract.type == "Direct"){
    volume.data$Total.Volume  <- volume.data[,paste0(legs[1],".volume")]   
    volume.data$Long.Volume   <- volume.data[,paste0(legs[1],".askvolume")] 
    volume.data$Short.Volume  <- volume.data[,paste0(legs[1],".bidvolume")] 
  }
  
  volume.data$LS.Imbalance       <- volume.data$Long.Volume - volume.data$Short.Volume
  volume.data$LS.Imbalance.Ratio <- volume.data$LS.Imbalance/abs(mean(volume.data$LS.Imbalance))
  
  volume.data$imb.sell.trigger  <- 0
  volume.data$imb.buy.trigger   <- 0
  
  ##find the extreme values of the imbalance ratio
  IB.Min <- quantile(volume.data$LS.Imbalance.Ratio, probs = c(1:100)/100)["1%"][[1]]
  IB.Max <- quantile(volume.data$LS.Imbalance.Ratio, probs = c(1:100)/100)["99%"][[1]]
  
  volume.data$imb.sell.trigger[which(volume.data$LS.Imbalance.Ratio < IB.Min)] <- 1
  volume.data$imb.buy.trigger[which(volume.data$LS.Imbalance.Ratio >  IB.Max)] <- 1 
  
  return(volume.data)
}
