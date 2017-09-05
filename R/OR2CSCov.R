#'
#' Outright to Calendar Spread Covariance Converter
#'
#' Convert Outright Covariance to a Calendare Spread Covariance
#'
#' Takes an outright covariance with consecutive contracts and creates the calendar spread
#' covariance matrix
#'
#'@param ORCov A covariance matrix of outright contracts.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

OR2CSCov <- function(ORCov){
  n <- nrow(ORCov)
  CSCov <- matrix(nrow = n - 1, ncol = n - 1)
  RowCol <- 'Filler'
  
  for(i in 1:(n - 1)){
    RowCol <- append(RowCol, paste(rownames(ORCov)[i], rownames(ORCov)[i + 1], sep ="-"))
    for(j in (i + 1):n){
      if(i == j - 1){
        CSCov[i, j - 1] <- ORCov[i, i] + ORCov[j, j] - 2 * ORCov[i, j]
      }else{
        CSCov[i, j - 1] <- CSCov[j - 1, i] <- ORCov[j - 1, i] + ORCov[j, i + 1] - ORCov[j - 1, i + 1] - ORCov[j, i]
      }
    }
  }
  rownames(CSCov) <- colnames(CSCov) <- RowCol[-1]
  
  return(CSCov)
}