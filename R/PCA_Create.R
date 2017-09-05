#'
#' Create PCA Matrix
#'
#' Calculates and records a matrix comprising of the principal component eigenvalues and
#' eigenvectors.
#'
#' Using an given outright covariance and a specified number of dimensions to use, this function 
#' will calculate the principal components returning a matrix with the first column corresponding
#' to the eigenvalues and the remaining columns the prespecified number of eigenvectors.
#'
#'@param OR_Cov A covariance matrix of outright contracts.
#'@param Dimension The number of eigenvectors to return.
#'
#'@author Nicholas Dregne
#'
#'@export

PCA_Create <- function(OR_Cov, Dimension){
  TradeDim <- c((ncol(OR_Cov) - (Dimension - 1)): ncol(OR_Cov))
  PCA      <- cbind(as.matrix(eigen(OR_Cov[TradeDim, TradeDim])$values), 
                    as.matrix(eigen(OR_Cov[TradeDim, TradeDim])$vectors))
  rownames(PCA) <- rownames(OR_Cov)[TradeDim]
  
  if(sum(PCA[,2]) < 0){
    PCA[,2] <- -PCA[,2]
  }
  
  if(sum(PCA[1:5,3]) < 0){
    PCA[,3] <- -PCA[,3]
  }
  
  if(sum(PCA[1:2,4]) < 0){
    PCA[,4] <- -PCA[,4]
  }
  
  if(sum(PCA[1,5]) < 0){
    PCA[, 5] <- -PCA[, 5]
  }
  
  if(sum(PCA[1,6]) < 0){
    PCA[, 6] <- -PCA[, 6]
  }
  
  return(PCA)
}