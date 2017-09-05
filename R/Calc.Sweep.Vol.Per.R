#'
#' Calculate the Inflation Factor From Market Sweeps.
#'
#'@param contract Contract to get sweep inflation factor.
#'@param start Start Date for analytical period.
#'@param end  End Date for analytical period
#'@param book.per Percentile of book size that is considered liquid.
#'
#'@author Helen Ristov and Nic Dregne
#'
#'@export
#'
#'@import data.table
#'

Calc.Sweep.Vol.Per <- function(contract, start, end, book.per, Limit, VML) {
  
  data <- try(Pull.TRS.Data(contract, start, end, incr = 0)[[1]], silent = TRUE)
  if(is(data,'try-error')){ return(1.0) }
  bs.p <- as.numeric(quantile(c(data$BidQty, data$AskQty), prob = book.per))
  
  data$SweepVolume <-
    ifelse(data$TradedPrice <= data$BestBid, 
           ## Trade on Bid 
           ifelse(data$TradedPrice == data$BestBid,
                  ## Check if Bid had Volume and the Trade Exceeded Sweep Limit
                  ifelse(data$BidQty > bs.p & data$TradedVolume > min(VML, Limit * data$BidQty), 
                         data$TradedVolume, 0),
                  ## Trade is Below Bid Price making all volume sweep volume
                  data$TradedVolume),
           ## Trade on Ask
           ifelse(data$TradedPrice >= data$BestAsk, 
                  ifelse(data$TradedPrice == data$BestAsk,
                         ## Check if Bid had Volume and the Trade Exceeded Sweep Limit
                         ifelse(data$AskQty > bs.p & data$TradedVolume > min(VML, Limit * data$AskQty), 
                                data$TradedVolume, 0),
                         ## Trade is Below Bid Price making all volume sweep volume
                         data$TradedVolume),
                  ## No Trade Occurred
                  0))
  
  sweep.per <- 100*sum(data$SweepVolume, na.rm = TRUE) / sum(data$TradedVolume, na.rm = TRUE)
  inf.fact  <- as.numeric(sqrt(1 + max(0, (sweep.per - 10) / 3 * 0.10 )))
  
  return(inf.fact)
}
