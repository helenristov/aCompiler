#'
#' Create PC Component Variable
#'
#' Calculates and records variables denoting the desired PC Eigenvectors' fluctuations.
#'
#' Using an given price series and an option eigenvector matrix (will create it from price series otherwise), this function 
#' will calculate the principal components' variation throughout the price series.
#'
#'@param returns A stationary price series whose variation through time we wish to examine.
#'@param dim Optional input for the eigenvectors for which we wish to calculate the PC variable.  If left null, then all possible vector variables will be return.
#'@param evMatrix An optional eigenvector matrix to use in generating the PC variables.
#'
#'@author Nicholas Dregne
#'
#'@export

calcPC.Vector <- function(returns, dim = NULL, evMatrix = NULL){
  if(!is.xts(returns)){ stop('Must have an xts class for the returns input for calcPC.Vector.') }
  if(is.null(dim)){ if(is.null(evMatrix)){ dim <- c(1:ncol(returns)) }else{ dim <- c(1:ncol(evMatrix)) }}
  if(is.null(evMatrix)){ evMatrix <- PCA_Create(covClassic(returns)$cov, ncol(returns))[,-1] }
  
  PCs <- as.xts(returns %*% evMatrix[,dim], order.by = index(returns))  
  
  return(PCs)
}