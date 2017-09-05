#' Calculates the SemiVariance of Price Series
#'
#'@param data A price series on which you want to compute the semivariance
#'@param split.num  The number to split the price series distribution.  If null, the mean is used.
#'@param n The number of observations to use in the calculation.  If null, the total set is combined.
#'
#'@author Nicholas Dregne
#'
#'@export


calcSemiVar <- function(data, split.num = NULL, n = NULL){
  
  if(is.null(n)){
    if(is.null(split.num)){ split.num <- mean(data) }
    
    SemiVar <- c(ifelse(length(which(data > split.num)) > 0, var(data[which(data > split.num),]), NA),
                 ifelse(length(which(data > split.num)) > 0, length(which(data > split.num))    , 0 ), 
                 ifelse(length(which(data < split.num)) > 0, var(data[which(data < split.num),]), NA), 
                 ifelse(length(which(data < split.num)) > 0, length(which(data < split.num))    , 0 ))
    names(SemiVar) <- c('Up.Var', 'Up.Obs', 'Down.Var', 'Down.Obs')
    
  }else{
    if(is.null(split.num)){ 
      split.num <- runMean(data, n = n, cumulative = FALSE) 
    }else if(length(split.num) == 1){
      split.num <- rep(split.num, nrow(data))
    }else if(length(split.num) != nrow(data)){
      stop("split.num must either be 1 number or the same number as the rows in the data variable.")
    } 
  
    Up.Var <- Up.Obs <- Down.Var <- Down.Obs <- rep(NA, n - 1)
    
    for(i in n:nrow(data)){
      Up.Var   <- append(Up.Var  , ifelse(length(which(data[c((i-n+1):i),] > split.num[i])) > 0, var(data[which(data[c((i-n+1):i),] > split.num[i]),]), NA))
      Up.Obs   <- append(Up.Obs  , ifelse(length(which(data[c((i-n+1):i),] > split.num[i])) > 0, length(which(data[c((i-n+1):i),] > split.num[i]))    , 0 ))
      Down.Var <- append(Down.Var, ifelse(length(which(data[c((i-n+1):i),] < split.num[i])) > 0, var(data[which(data[c((i-n+1):i),] < split.num[i]),]), NA))
      Down.Obs <- append(Down.Obs, ifelse(length(which(data[c((i-n+1):i),] < split.num[i])) > 0, length(which(data[c((i-n+1):i),] < split.num[i]))    , 0 ))
    }
    
    SemiVar <- cbind(Up.Var, Up.Obs, Down.Var, Down.Obs)
    colnames(SemiVar) <- c('Up.Var', 'Up.Obs', 'Down.Var', 'Down.Obs')
  }
  
  return(SemiVar)
}  
