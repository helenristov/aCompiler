#'
#' Calculates Amount of Risk Reduced Based on an Outright Covariance Matrix 
#'
#' Calculates Portfolio Risk Reduction for Pre-Selected Contract Types
#'
#'@param cov Outright covariance matrix to be used in the variance calculations
#'@param pos A vector of outright positions for which we wish to know the equivalent positions.
#'@param types Types of combinations to be calculated from the outright covariance matrix.
#'@param ref.qty Reference quantity used to determine what a full traded level would do to risk.
#'@param ref.contract The contract that should be used as the basis for reference volatility.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export


CalcRiskRedux <- function(cov, pos, types, delta, ref.qty = 1, ref.contract = NULL){
  
  if(!(delta %in% c('EQ.Pos', 'Lvl.Qty'))){ stop("Delta can only be 'EQ.Pos' or 'Lvl.Qty.'") }
  if(is.null(ref.contract)){ ref.contract <- EGTypeNames(colnames(cov), types[1])[min(5,length(EGTypeNames(colnames(cov), types[1])))] }
  
  port.var <- t(as.matrix(as.numeric(pos))) %*% cov %*% as.matrix(as.numeric(pos))
    vars <- CalcVar(cov, types)   
  refvar <- vars[[getContractType(ref.contract)]][which(rownames(vars[[getContractType(ref.contract)]]) == ref.contract)]
  
  Risk.Redux <- CalcEQPos(cov, pos, types)
  
  for(i in 1:ncol(Risk.Redux)){
    contracts <- EGTypeNames(rownames(cov), colnames(Risk.Redux)[i])
    for(j in 1:length(contracts)){
      if(is.na(Risk.Redux[j, i])){ next }
      
      legs <- as.character(getContractLegs(contracts[j])$Legs)
      ratio<- as.numeric(getContractLegs(contracts[j])$Ratio)
         
      contract.var <- t(as.matrix(ratio)) %*% cov[legs, legs] %*% as.matrix(ratio)
      contract.qty <- round(max(1, ref.qty / sqrt(vars[[getContractType(contracts[j])]][which(rownames(vars[[getContractType(contracts[j])]]) == contracts[j])] / refvar)), 0)
         delta.qty <- ifelse(delta == 'EQ.Pos', -round(Risk.Redux[j, i], 0), -round(sign(Risk.Redux[j, i]) * contract.qty, 0))
      
      Risk.Redux[j, i] <- sqrt(port.var + delta.qty^2*contract.var + 2 * delta.qty * (t(as.matrix(as.numeric(pos))) %*% (cov[,legs] %*% as.matrix(ratio)))) - sqrt(port.var)
    }
  }    
  
  return(Risk.Redux)
}

