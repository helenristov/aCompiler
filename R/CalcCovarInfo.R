#' Calculates Covariates for EQPos and Risk Redux Calcs
#'
#'@param cov Covariance matrix from which covariates will be derived.
#'@param spread Spread for which to get covariates.
#'@param raw Optional parameter to return raw covarinputs for contracts with an non-one front beta.
#'
#'@author Nicholas Dregne
#'
#'@export
#'
#'
## Measure used to calc WMP
CalcCovarInfo <- function(cov, spread, raw = FALSE, date = last(GetWeekDays(Sys.Date() - 8, Sys.Date() - 1))){
  
  Contracts <- colnames(cov)
  
  type <- getContractType(spread)
  vars <- CalcVar(cov, type, date)[[type]][spread,]
  
  legs  <- getContractLegs(spread, date)$Legs
  ratio <- t(as.matrix(getContractLegs(spread, date)$Ratio))
  
  Covariates <- list()
  for(C in Contracts){ Covariates[[C]] <- list(Name = C, Value = (ratio %*% cov[legs, C]) / ifelse(raw, 1, as.numeric(ratio[1,1]))) }
  
  Covariates[[length(Covariates) + 1]] <- list(Name = "ContractVariance", Value = vars)
  names(Covariates) <- rep('Contract', length(Covariates))
  
  return(Covariates)
}