#'
#' Calculates the number of increments for a specified window of time
#'
#'@param start.date The beginning of the data pull window
#'@param end.date  The ending of the data pull window
#'@param incr      The bar length of the data pull
#'@param window.size The total length of the window in the units specified 
#'@param window.units The time units. Supported values are secs, mins, hours, days 
#'@author Helena Ristov
#'
#'@export

IncrPerWindow  <- function(start.date, end.date, incr, window.size, window.size.units){ 
  
  temp1 <- as.POSIXct(paste0(Sys.Date(), strsplit(start.date," ")[[1]][2], " ", strsplit(start.date," ")[[1]][3]))
  
  datefactor <- ifelse(as.POSIXlt(temp1)$hour > 12, 1, 0)
  
  temp2 <- as.POSIXct(paste0(Sys.Date()+datefactor, strsplit(end.date," ")[[1]][2], " ", strsplit(end.date," ")[[1]][3]))
  
  secsPerTradeDay <- as.numeric(difftime(temp2, temp1, tz, units = 'secs'))
  
  if(window.size.units == 'mins'){
    IncrPerWindow <- 60*window.size/incr
  }else if(window.size.units == 'hours'){
    IncrPerWindow <- 60*60*window.size/incr
  }else if(window.size.units == 'days'){
    IncrPerWindow <- secsPerTradeDay*window.size/incr
  }else if(window.size.units == 'secs'){
    IncrPerWindow <- window.size
  }else{stop('window size not supported in IncrPerWindow')}
  #     IncrPerUnit    <-  max(sapply(unique( trunc.POSIXt(index(data), units = window.size.units)), function(x){  length(which(trunc.POSIXt(index(data), units = window.size.units) == x))}))
  #   }else{
  #     IncrPerUnit    <-  length(which(trunc.POSIXt(index(data), units = window.size.units) == trunc.POSIXt(index(data), units = window.size.units)[2]))
  #   }
  #     IncrPerWindow  <-  window.size*IncrPerUnit
  #   
  return(IncrPerWindow)
}