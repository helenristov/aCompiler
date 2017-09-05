#'
#' Incr Counter
#'
#' Calculates the number of times a time-series of prices has moved a pre-specified increment.
#'
#' Given a time-series of prices and an increment level, the function will determine how many
#' times the price series has moved the specified increment level before reverting back a full
#' increment level.
#'
#'@param Prices The time-series of prices.
#'@param incr The desired increment level.
#'
#'@author Nicholas Dregne
#'
#'@export

IncrCounter <- function(Prices, incr){
  
  Adj_Prices <- Prices / incr
  Base_Price <- as.numeric(Adj_Prices[1,])
  TickRows   <- 1
  TickMoves  <- 0
  
  ## Calculate all tick moves and list them as a series
  for(i in 2:nrow(Adj_Prices)) {
    if(abs(as.numeric(Adj_Prices[i, ]) - Base_Price) >= 1){
      TickMove   <- trunc(as.numeric(Adj_Prices[i,]) - Base_Price)
      
      TickRows   <- append(TickRows, i)
      TickMoves  <- append(TickMoves, TickMove)
      Base_Price <- Base_Price + TickMove
    }
  }
  
  n <- length(TickMoves)
  TickCount <- 99999
  
  ## Check if there are any tick moves.  If none, then calculate drift, loss and other output measures.
  if (n < 2 || length(which(TickMoves[-1] > 0)) < 1 || length(which(TickMoves[-1] < 0)) < 1){
    return(sum(TickMoves))
  }
  
  ## If there are tick moves, loop through and calculate the total moves.
  for (i in 2:n){
    if (i == 2) {
      if(sign(TickMoves[i]) != sign(TickMoves[i + 1])){
        TickCount <- TickMoves[i]
        sum <- 0
      }else{
        sum <- TickMoves[i]
      }
    }else if (i == n){
      if (sign(TickMoves[i]) != sign(TickMoves[i - 1])) {
        TickCount <- append(TickCount, TickMoves[i])
      }else{
        TickCount <- append(TickCount, sum + TickMoves[i])
      }
    }else{
      if(sign(TickMoves[i]) != sign(TickMoves[i + 1])){
        if (TickCount[1] == 99999){
          TickCount <- sum + TickMoves[i]
        }else{
          TickCount <- append(TickCount, sum + TickMoves[i])
        }
        
        sum <- 0
      }else{
        sum <- sum + TickMoves[i]
      }
    }
  }

  return(TickCount)
}
