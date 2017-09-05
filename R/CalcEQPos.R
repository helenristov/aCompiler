#'
#' Calculates Equivalent Positions Based on an Outright Covariance Matrix 
#'
#' Calculates EQ Pos for Pre-Selected Contract Types
#'
#'@param cov Outright covariance matrix to be used in the variance calculations
#'@param pos A vector of outright positions for which we wish to know the equivalent positions.
#'@param types Types of combinations to be calculated from the outright covariance matrix.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export


CalcEQPos <- function(cov, pos, types){
  
  vars <- CalcVar(cov, types)
   pos <- as.matrix(pos)
  
  EQ.Betas <- list()
  for(type in types){
    EQ.Betas[[type]] <- matrix(nrow = nrow(vars[[type]]), ncol = ncol(cov))
    rownames(EQ.Betas[[type]]) <- rownames(vars[[type]])    
    colnames(EQ.Betas[[type]]) <- colnames(cov)
    
    for(spd in rownames(vars[[type]])){
      legs  <- getContractLegs(spd)$Legs
      ratio <- t(as.matrix(getContractLegs(spd)$Ratio))
      
      EQ.Betas[[type]][spd,] <- ratio %*% cov[legs,] / vars[[type]][spd,]
    }
  }
  
  EQ.Pos.Matrix <- matrix(nrow = max(unlist(lapply(vars, length))), ncol = length(types))
  colnames(EQ.Pos.Matrix) <- types
  rownames(EQ.Pos.Matrix) <- rownames(cov)[1:max(unlist(lapply(vars, length)))]
  
  for(type in types){
    EQ.Pos.Matrix[1:length(vars[[type]]), type] <- round(as.numeric(EQ.Betas[[type]] %*% pos), 2)
  }
  
  return(EQ.Pos.Matrix)
}