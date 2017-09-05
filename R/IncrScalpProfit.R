#'
#' Volatilty Scalp Profit
#'
#' Internal function that calculates an increment profit given a specific increment count
#'
#'@param TickCount An increment count series.
#'@param MaxLevels Maximum number of levels that can be taken on 
#'@param slippage Optional factor that penalizes the assumption of perfect execution by a percentage of the increment
#'
#'@author Nicholas Dregne
#'
#'@export


IncrScalpProfit <- function(TickCount, MaxLevels, slippage = 0){
  TickTurns  <- c(0,TickCount)
  PriceTurns <- c(0,cumsum(TickCount))
  
  PosCalc <- function(PrevPos, DeltaTicks, CumTicks, MaxLevels){
    if(abs(CumTicks) > MaxLevels && sign(CumTicks) == sign(DeltaTicks)){
      if(abs(PrevPos) < MaxLevels){
        return(-sign(CumTicks) * min(abs(PrevPos - DeltaTicks), MaxLevels))
      }else{
        return(-sign(CumTicks) * MaxLevels)
      }
    }else{
      return(PrevPos - DeltaTicks)
    }    
  }
  
  DeltaProfitCalc <- function(PrevPos, DeltaTicks, CumTicks, MaxLevels){
    if(CumTicks == 0){
      Profit <- abs(PrevPos)
    }else if(sign(PrevPos) == sign(DeltaTicks)){
      Profit <- min(abs(DeltaTicks), abs(PrevPos))
    }else{
      Profit <- 0
    }
    
    if(abs(PrevPos) > MaxLevels && -sign(PrevPos) == sign(DeltaTicks)){
      Loss <- abs(DeltaTicks) - abs(PrevPos)
    }else if(abs(PrevPos - DeltaTicks) > MaxLevels){
      Loss <- abs(PrevPos - DeltaTicks) - MaxLevels
    }else{
      Loss <- 0
    }
    
    return(Profit - Loss * MaxLevels)
  }
  
  Pos         <- 0
  DeltaProfit <- 0
  
  for(i in 2:length(TickTurns)){
    Pos         <- append(Pos        , PosCalc(        Pos[i-1], TickTurns[i], PriceTurns[i], MaxLevels))
    DeltaProfit <- append(DeltaProfit, DeltaProfitCalc(Pos[i-1], TickTurns[i], PriceTurns[i], MaxLevels))
  
    if(i == length(TickTurns)){
      n              <- max(abs(Pos[i]) - 1, 0)
      DeltaProfit[i] <- DeltaProfit[i] - (n * (n + 1)) / 2
      Pos[i]         <- 0
    }
  }
  
  M2MPnL <- (sapply(abs(Pos), function(x){max(abs(x) - 1, 0)}) * 
            (sapply(abs(Pos), function(x){max(abs(x) - 1, 0)}) - 1)) / 2
  
  Output <- list(GrossPnL    = last(cumsum(DeltaProfit)) - slippage * sum(abs(diff(PriceTurns))), 
                 Trades      = sum(abs(diff(PriceTurns))),   
                 MaxDrawdown = min(cumsum(DeltaProfit) - M2MPnL),
                 HighWater   = max(cumsum(DeltaProfit) - M2MPnL))
  
  return(Output)
}
