#'
#' Incr Capture
#'
#' Internal Function That Calculates a Incr Profit Given a Specific Incr Size
#' 
#' This calculation is very crude as it assumes all increments can be held forever and there is no puke point.
#'
#'@param TickCount A tick count series.
#'@param slippage Optional factor that penalizes the assumption of perfect execution by a percentage of the increment
#'
#'@author Nicholas Dregne
#'


IncrCapture <- function(TickCount, slippage = 0.00){
  TickTurns  <- c(0,TickCount)
  PriceTurns <- c(0,cumsum(TickCount))
  AvgBuy  <- 0
  BuyQty  <- 0
  AvgSell <- 0
  SellQty <- 0
  
  for(i in 2:length(TickTurns)){
    if(TickTurns[i] < 0){
      AvgBuy <- (AvgBuy * BuyQty + abs(TickTurns[i]) * (PriceTurns[i] - 1 + PriceTurns[i-1]) / 2.0) / (BuyQty + abs(TickTurns[i]))
      BuyQty <- BuyQty + abs(TickTurns[i])
    }else{
      AvgSell <- (AvgSell * SellQty + TickTurns[i] * (PriceTurns[i] + 1 + PriceTurns[i-1]) / 2.0) / (SellQty + TickTurns[i])
      SellQty <- SellQty + TickTurns[i]
    }
  }
  
  if(BuyQty > SellQty){
    AvgSell <- (AvgSell * SellQty + PriceTurns[length(PriceTurns)] * (BuyQty - SellQty)) / BuyQty
    PnL <- (AvgSell - AvgBuy) * BuyQty
  }else if(BuyQty < SellQty){
    AvgBuy <- (AvgBuy * BuyQty + PriceTurns[length(PriceTurns)] * (SellQty - BuyQty)) / SellQty
    PnL <- (AvgSell - AvgBuy) * SellQty
  }else{
    PnL <- (AvgSell - AvgBuy) * BuyQty
  }
  
  PnL <- PnL - 2 * slippage * BuyQty
  
  return(PnL)
}
