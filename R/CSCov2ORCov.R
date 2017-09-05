#'
#' Transform Calendar Spread Covariance to Outright Covariance
#'
#' Taking a covariance matrix of a single outright and multiple calendar spreads, this function will create a covariance matrix of all outrights.
#'
#' This function, using the covariance identities, recreates what all outright covariances are based on the covariances
#' of the contracts' calendar spreads which tend to be more liquid and less volatile.  Note: Only one asset class can be used with this function.
#'
#'@param CS_CovMat The calendar spread covariance matrix where the first contract is the single outright required.
#'@param ContractSymbol The symbol of the underlying asset class.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

CSCov2ORCov <- function(CS_CovMat, ContractSymbol){
  # The Outright used to imply all other outrights must be the first contract in the CS_CovMat and
  # the nearest to maturity contract.
  
  if(class(CS_CovMat) != "matrix"){
    stop("Need to Input a Covariance Matrix")
  }
  
  if(is.na(sum(CS_CovMat))){
    stop("Covariance Matrix is Not Fully Defined.  NAs are Present")
  }
  
  SymbolLen <- nchar(ContractSymbol) 
  
  ContractNames <- substr(rownames(CS_CovMat)[1], SymbolLen + 1, SymbolLen + 2)
  ContractNames <- c(ContractNames, substr(rownames(CS_CovMat)[-1], SymbolLen + 4, SymbolLen + 5))
  
  OR_CovMat <- as.matrix(CS_CovMat)
  n <- nrow(CS_CovMat)
  
  colnames(OR_CovMat) <- paste(ContractSymbol, ContractNames, sep="")
  rownames(OR_CovMat) <- paste(ContractSymbol, ContractNames, sep="")
  
  for(i in 1:n){
    for(j in i:n){
      FrontOR <- which(rownames(OR_CovMat) == substr(rownames(CS_CovMat)[j],  1,  SymbolLen + 2))
      BackOR  <- which(rownames(OR_CovMat) == paste(ContractSymbol, substr(rownames(CS_CovMat)[i],  SymbolLen + 4,  SymbolLen + 5), sep=""))
      
      if(i == 1){
        if(j == 1){
          OR_CovMat[1, 1] <- CS_CovMat[1, 1]
        }else{
          OR_CovMat[1, j] <- OR_CovMat[j, 1] <- OR_CovMat[FrontOR, 1] - CS_CovMat[j, i]
        }
      }else{
        if(j == i){
          OR_CovMat[j, i] <- CS_CovMat[j, j] - OR_CovMat[FrontOR, FrontOR] + 2 * OR_CovMat[j, FrontOR]
        }else{
          OR_CovMat[i, j] <- OR_CovMat[j, i] <- CS_CovMat[j, i] + OR_CovMat[j, i - 1] + OR_CovMat[FrontOR, i] - OR_CovMat[FrontOR, i - 1]
        }
      }
    }
  }
  
  return(OR_CovMat)
}