#' Computes the Kalman Filter model values using the DLM Polynomial Fit 
#'
#'@param data A price series 
#'@param order.model The order of the polynomial for the dlmModPoly approximation and smoothing
#'@param dV  The observation variance to be used in the polynomial fit. The default value is set to "vol" which compute the variance of the price series. Other values can be inputed to
#'@param window The window for anchored or rolling fit mechanisms. The default is set to 0 since fit.type is defaulted to full.
#'@param fit.type  The different ways we can obtain the fitted values. The accepted type are full, anchored, and rolling. "full" parameterizes the model on the entire data range. Anchored sets a fixed window and every forward estimate uses all data from the start of the data vector. Rolling computes the next value from a rolling window for model parameterization. 
#'@param err.sd.lvl The error threshold at which we generate the buy and sell signals for the error correction model
#'@return list of values are returned
#' \item{dlm.fitted}{A vector of fitted model values}
#'@author Helena Ristov
#'
#'@export
#'
getKalmanFitted <- function(data, order.model = 2, dV = "vol", window = 0, fit.type = "full", err.sd.lvl = NULL){
  
  getDLM <- function(data){
    
   ##set the observation variance for the model
    if(dV == "vol"){
      vol  <- sd(diff(data), na.rm = TRUE)
    }else{
      vol <- dV
    }
    dlm.kfilter <- dlmModPoly(order=order.model, dV=vol^2, dW=c(rep(0,order.model-1),1), C0=1e+07*diag(nrow=order.model))
     # Apply the Kalman filter to get the fitted values
    dlm.result <- dlmFilter(y=coredata(data), mod=dlm.kfilter)
    dlm.fitted <- xts(as.matrix(dlm.result$m)[-1,1], order.by=index(data))
    return(dlm.fitted)
  }
  ## generate the model values on the different fit mechanisms that we can implement in production
  if(fit.type == "full"){
    dlm.fitted  <- getDLM(data)
  }else if(fit.type == "anchored"){
    data.anchor <- data[1:window,]
    
    ##Generate the values on the anchor window
    anchor.fit.values <- as.xts(getDLM(data.anchor))
    
    ##compute the incremental values off the anchor
    num <- nrow(data) - window
    incremental.values <- sapply(1:num, function(i){last(getDLM(data[1:(window+i)]))})
    incremental.values <- as.xts(incremental.values, order.by = index(data)[(window+1):nrow(data)])
    dlm.fitted <- rbind(anchor.fit.values, incremental.values)
  }else if(fit.type == "rolling"){
    static.data <- getDLM(data[1:window-1])
    roll.fit    <- rollapply(data, width = window, function(x){last(getDLM(x))})
    dlm.fitted  <- rbind(static.data, roll.fit[which(!is.na(roll.fit))]) 
  }  
  colnames(dlm.fitted) <- paste('filtered.',colnames(data),sep="")
  
  if(!is.null(err.sd.lvl)){
    vol  <- sd(diff(data), na.rm = TRUE)
    resid <- dlm.fitted - data
    dlm.fitted$buy.signal <- 0
    dlm.fitted$ask.signal <- 0
    index.buy  <- which(resid < -err.sd.lvl*vol)
    index.sell <- which(resid > err.sd.lvl*vol)
    dlm.fitted$buy.signal[index.buy] <- 1
    dlm.fitted$ask.signal[index.sell] <- 1
  }

  return(dlm.fitted)

}