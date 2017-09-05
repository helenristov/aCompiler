#'
#' Creates an Open, High, Low, Close time-series for a given contract.
#'
#'@param data MDR or TRS data.
#'@param contract The contract that you are pulling data for.
#'@param barLength The length of bar for OHLC calculation in seconds.
#'
#'@author Helen Ristov and Cameron Sawyer
#'
#'@export
#'
#'@import data.table
#'

Calc.OHLC <- function(data, contract, barLength) {
  
  # drop all data but traded price
  price <- data$TradedPrice
  
  # carry forward last observation for NAs
  price <- na.locf(price, na.rm = FALSE)
  price[is.na(price)] <- Inf
  
  # bin based on bar length
  price <- align.time(price, barLength)
  price <- data.table(index = as.POSIXct(index(price)), TradedPrice = as.numeric(price[,'TradedPrice']))
  
  # summarise into OHLC format
  price <- price[, list(open  = first(TradedPrice), 
                        high  = max(TradedPrice, na.rm = TRUE), 
                        low   = min(TradedPrice, na.rm = TRUE), 
                        close = last(TradedPrice)), by = "index"]
  
  price <- xts(subset(price, select = c("open", "high", "low", "close")), order.by = price$index)
  
  # make sure there is a bar for every minute
  SquareTime <- seq(start(price), end(price), barLength)
  price <- merge(price, SquareTime)
  
  # fill in the bars
  price[, c("close")]                         <- na.locf(price[, c("close")])
  price[is.na(price[, c("low")]), c("low")]   <- price[is.na(price[, c("low")]), c("close")]
  price[is.na(price[, c("open")]), c("open")] <- price[is.na(price[, c("open")]), c("close")]
  price[is.na(price[, c("high")]), c("high")] <- price[is.na(price[, c("high")]), c("close")]
  
  # replace Inf values with NA
  price[is.infinite(price)] <- NA
  
  return(price)
}
