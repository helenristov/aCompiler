#' Calculates Perceived Market Edge
#'
#'@param market.data calcMarketMeasures data set
#'@param contract Contract for which you desire to get edge measures.
#'@param start.lean Start lean value to determine lean factor for edge calc.
#'@param end.lean End lean value to determine lean factor for edge calc.
#'@param edge.floor Edge floor to use if we desire one.
#'@param min.edge.qty Min qty for edge floor calc.
#'@param max.edge.qty Max qty for edge floor calc.
#'@param TT_MinTick A boolean to determine if we should use the TT mintick or TRS mintick.
#'
#'@author Nicholas Dregne
#'
#'@export
#'
#'
## Measure used to calc WMP
calcBidAsk.Edge <- function(market.data, contract, start.lean = NULL, end.lean = NULL, edge.floor = NULL, min.edge.qty = NULL, max.edge.qty = NULL, TT_MinTick = FALSE){
  if(is.null(  start.lean)){   start.lean <- 1.0 }
  if(is.null(    end.lean)){     end.lean <- 1.0 }
  if(is.null(  edge.floor)){   edge.floor <- 0.0 }
  if(is.null(min.edge.qty)){ min.edge.qty <- 0.0 }
  if(is.null(max.edge.qty)){ max.edge.qty <- 0.0 }
  
  mt <- getContractMT(contract, TT_MinTick = TT_MinTick)
  if(substr(contract, 1, 2) %in% c('GE','ED')){
    HTDate <- as.POSIXlt(paste0(GEHalfTick(contract, as.Date(index(market.data)[1])) - 1, " ", getInfo(contract)$times$a.open))
    HTRows <- which(as.POSIXlt(index(market.data)) >= HTDate)
    
    if(length(HTRows) > 0){
      mt <- rep(mt, nrow(market.data))
      mt[HTRows] <- mt[1] / 2 
    }
  }
  
  ## Check to See if Lean Edge Needs to be Applied
  exec.ask.edge <- (market.data[,'WMP'] - market.data[,'BestBid']) / mt
  exec.bid.edge <- (market.data[,'BestAsk'] - market.data[,'WMP']) / mt
  
  Lean.Names <- character()
  for(sl in start.lean){
    for(el in end.lean){
      if(!exists("exec.ask.lean.edge")){
        exec.ask.lean.edge <- ifelse(1 - exec.ask.edge > el, 0, ifelse(1 - exec.ask.edge < sl, exec.ask.edge, 1 - sl - (1 - exec.ask.edge - sl) / (el - sl) * (1 - sl)))
        exec.bid.lean.edge <- ifelse(1 - exec.bid.edge > el, 0, ifelse(1 - exec.bid.edge < sl, exec.bid.edge, 1 - sl - (1 - exec.bid.edge - sl) / (el - sl) * (1 - sl)))  
      }else{
        exec.ask.lean.edge <- cbind(exec.ask.lean.edge, ifelse(1 - exec.ask.edge > el, 0, ifelse(1 - exec.ask.edge < sl, exec.ask.edge, 1 - sl - (1 - exec.ask.edge - sl) / (el - sl) * (1 - sl))))
        exec.bid.lean.edge <- cbind(exec.bid.lean.edge, ifelse(1 - exec.bid.edge > el, 0, ifelse(1 - exec.bid.edge < sl, exec.bid.edge, 1 - sl - (1 - exec.bid.edge - sl) / (el - sl) * (1 - sl))))        
      }
      
      Lean.Names <- append(Lean.Names, paste0("WMP.SL.", sl, ".EL.", el))
    }
  }
  colnames(exec.ask.lean.edge) <- colnames(exec.bid.lean.edge) <- Lean.Names
  
  ## Check to See if Edge Floor Needs to be Applied
  
  Final.Names <- character()
  for(i in 1:ncol(exec.ask.lean.edge)){
    for(ef in edge.floor){
      for(neq in min.edge.qty){
        for(xeq in max.edge.qty){
          if(!exists("exec.ask.edge.final")){
            exec.ask.edge.final <-  ifelse(market.data[,'BidQty'] >= xeq, pmax(ef, exec.ask.lean.edge[,i]), ifelse(market.data[,'BidQty'] <= neq, exec.ask.lean.edge[,i], pmax(exec.ask.lean.edge[,i], (market.data[,'BidQty'] - neq) / (xeq - neq) * ef)))
            exec.bid.edge.final <-  ifelse(market.data[,'AskQty'] >= xeq, pmax(ef, exec.bid.lean.edge[,i]), ifelse(market.data[,'AskQty'] <= neq, exec.bid.lean.edge[,i], pmax(exec.bid.lean.edge[,i], (market.data[,'AskQty'] - neq) / (xeq - neq) * ef)))          
          }else{
            exec.ask.edge.final <-  cbind(exec.ask.edge.final, ifelse(market.data[,'BidQty'] >= xeq, pmax(ef, exec.ask.lean.edge[,i]), ifelse(market.data[,'BidQty'] <= neq, exec.ask.lean.edge[,i], pmax(exec.ask.lean.edge[,i], (market.data[,'BidQty'] - neq) / (xeq - neq) * ef))))
            exec.bid.edge.final <-  cbind(exec.bid.edge.final, ifelse(market.data[,'AskQty'] >= xeq, pmax(ef, exec.bid.lean.edge[,i]), ifelse(market.data[,'AskQty'] <= neq, exec.bid.lean.edge[,i], pmax(exec.bid.lean.edge[,i], (market.data[,'AskQty'] - neq) / (xeq - neq) * ef))))        
          }          
          
          Final.Names <- append(Final.Names, paste0(colnames(exec.ask.lean.edge)[i], ".EF.", ef, ".NEQ.", neq, ".XEQ.", xeq))
        }
      }
    }    
  }
  if(length(start.lean) == 1 && length(end.lean) == 1 && length(edge.floor) == 1 && length(min.edge.qty) == 1 && length(max.edge.qty) == 1){
    colnames(exec.ask.edge.final) <- 'Ask.Edge'
    colnames(exec.bid.edge.final) <- 'Bid.Edge'
  }else{
    colnames(exec.ask.edge.final) <- sub('WMP.', 'EAE.', Final.Names)
    colnames(exec.bid.edge.final) <- sub('WMP.', 'EBE.', Final.Names)  
  }
  
  market.data <- cbind(market.data, exec.ask.edge.final, exec.bid.edge.final)
  
  return(market.data)
}