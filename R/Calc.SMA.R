#'
#' Creates start and end datetimes for pullling data
#'
#'@param data MDR or TRS data.
#'@param contract The contract that you are pulling data for.
#'@param start.date The start date of the SMA
#'@param end.date The end date of the SMA
#'@param incr Time bucket increment of SMA obs
#'@param window.size Number of units in SMA
#'@param window.size.units The unit of measure for the SMA length
#'
#'@author Helen Ristov
#'
#'@export

Calc.SMA <- function(data, contract, start.date, end.date, incr, window.size, window.size.units){
  
  Increments    <- IncrPerWindow(start.date, end.date, incr, window.size, window.size.units)
  SMA           <- SMA(data, n = floor(Increments))
  SMA           <- SMA[-which(is.na(SMA[,1])),]
  colnames(SMA) <- c(paste0(contract, ".", "SMA.", window.size, window.size.units))
  
  assign(paste0("SMA.", window.size, window.size.units), SMA)
  return(SMA)  
}
