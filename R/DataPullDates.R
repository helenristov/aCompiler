#'
#' Creates start and end datetimes for pullling data
#'
#'@param contract The contract that you are pulling data for.
#'@param tradeDays The number of trade days of data to pull.
#'@param endDate The last day you wish to pull data for. Defaulted to yesterday.
#'
#'@author Cameron Sawyer
#'
#'@export

dataPullDates <- function(contract, tradeDays, endDate = Sys.Date() - 1, type = "A") {
  
  if(!type %in% c('A','M')){ stop("Need a valid open/close time for dataPullDates.") }
  
  weekDays <- ceiling(tradeDays * (7 / 5))
  
  dates <- GetWeekDays(StartDate = as.Date(endDate) - weekDays, EndDate = as.Date(endDate))
  dates <- dates[(length(dates)-tradeDays):length(dates)]
  
  if (length(dates) == tradeDays) {
    startDate = first(dates)
    endDate = last(dates)
  } else if ((length(dates) - 1) == tradeDays){
    startDate = ifelse(tradeDays == 0, dates[1], first(dates[-1]))
    endDate = last(dates)
  } else if ((length(dates) - 2 == tradeDays)) {
    startDate = first(dates[-2])
    endDate = last(dates)
  } else {
    stop("An unexpected error occured in pullDateTime.")
  }
  
  startDate <- as.character(as.Date(startDate) - 1)
  
  start <- paste0(startDate, " ", getInfo(contract)$times[[ifelse(type=='A', 'a.open' , 'm.open' )]])
  end   <- paste0(endDate  , " ", getInfo(contract)$times[[ifelse(type=='A', 'a.close', 'm.close')]])
  
  return(list(start = start, end = end))
}