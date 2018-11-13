#'
#' Calculates Variances for Different Contract Types Based on an Outright Covariance Matrix 
#'
#' Calculates Variances for Pre-Selected Contract Types
#'
#' CalcVar can calculate variances from an outright covariance matrix for outrights, calendar spreads, butterflies, double butterflies, packs, pack spreads and bundles.
#'
#'@param OR_Cov Outright covariance matrix to be used in the variance calculations
#'@param Types Types of combinations to be calculated from the outright covariance matrix.
#'@param date optional parameter in case of ICS var calcs.  Need a date to determine the TLS beta to use.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

CalcVar <- function (OR_Cov, Types, date = last(GetWeekDays(Sys.Date() - 8, Sys.Date() - 1))){
  
  OR_Contracts <- rownames(OR_Cov)
  Root <- substr(OR_Contracts[1], 1, nchar(OR_Contracts[1]) - 2)
  RL   <- nchar(Root)
  RL1  <- RL + 1
  
  Var <- list()
  
  for(Type in Types){
    
    TL  <- nchar(Type)
    
    if('OR' %in% Type){
      
      Var$OR <- as.matrix(diag(OR_Cov))
      rownames(Var$OR) <- EGTypeNames(OR_Contracts, Type)
      colnames(Var$OR) <- 'OR_Vars'
      
    }else if('CS' %in% substr(Type, 1, 2) && length(which(c(1:24) %in% substr(Type, 3, TL))) > 0){
      
      m <- as.numeric(substr(Type, 3, TL))
      n <- nrow(OR_Cov) - m
      
      if(n < 1){stop(paste0("Not enough contracts in OR_Cov for ", Type))}
      
      Spread <- matrix(trunc(seq(from = 1, to = -1, length.out = m + 1), 0), nrow = m + 1, ncol = 1, byrow = TRUE)
      
      Var[[paste0("CS", m)]] <- matrix(nrow = n, ncol = 1)
      rownames(Var[[paste0("CS", m)]]) <- EGTypeNames(OR_Contracts, Type)
      colnames(Var[[paste0("CS", m)]]) <- paste0("CL", m, "_Vars")
      
      for(i in 1:n){
        Var[[paste0("CS", m)]][i, 1] <- t(Spread) %*% OR_Cov[(i:(i + m)), (i:(i + m))] %*% Spread
      }
      
    }else if('FL' %in% substr(Type, 1, 2) && length(which(c(1:12) %in% substr(Type, 3, TL))) > 0){
      
      m <- as.numeric(substr(Type, 3, TL))
      n <- nrow(OR_Cov) - m * 2
      
      if(n < 1){stop(paste0("Not enough contracts in OR_Cov for ", Type))}
      
      Spread <- rep(0, m * 2 + 1)
      Spread[c(1, length(Spread))] <- 1
      Spread[trunc(length(Spread) / 2, 0) + 1] <- -2
      Spread <- matrix(Spread, nrow = m * 2 + 1, ncol = 1, byrow = TRUE)
      
      Var[[paste0("FL", m)]] <- matrix(nrow = n, ncol = 1)
      rownames(Var[[paste0("FL", m)]]) <- EGTypeNames(OR_Contracts, Type)
      colnames(Var[[paste0("FL", m)]]) <- paste0("FL", m, "_Vars")
      
      for(i in 1:n){
        Var[[paste0("FL", m)]][i, 1] <- t(Spread) %*% OR_Cov[(i:(i + m * 2)), (i:(i + m * 2))] %*% Spread
      }
      
    }else if('CN' %in% substr(Type, 1, 2) && length(which(c(1:12) %in% substr(Type, 3, TL))) > 0){
      
      m <- as.numeric(substr(Type, 3, TL))
      n <- nrow(OR_Cov) - m * 3
      
      if(n < 1){stop(paste0("Not enough contracts in OR_Cov for ", Type))}
      
      Spread <- rep(0, m * 3 + 1)
      Spread[c(1, length(Spread))] <- 1
      Spread[c(m + 1, m * 2 + 1)] <- -1
      Spread <- matrix(Spread, nrow = m * 3 + 1, ncol = 1, byrow = TRUE)
      
      Var[[paste0("CN", m)]] <- matrix(nrow = n, ncol = 1)
      rownames(Var[[paste0("CN", m)]]) <- EGTypeNames(OR_Contracts, Type)
      colnames(Var[[paste0("CN", m)]]) <- paste0("CN", m, "_Vars")
      
      for(i in 1:n){
        Var[[paste0("CN", m)]][i, 1] <- t(Spread) %*% OR_Cov[(i:(i + m * 3)), (i:(i + m * 3))] %*% Spread
      }
      
    }else if('DF' %in% substr(Type, 1, 2) && length(which(c(1:12) %in% substr(Type, 3, TL))) > 0){
      
      m <- as.numeric(substr(Type, 3, TL))
      n <- nrow(OR_Cov) - m * 3
      
      if(n < 1){stop(paste0("Not enough contracts in OR_Cov for ", Type))}
      
      Spread <- rep(0, m * 3 + 1)
      Spread[1]              <-  1
      Spread[m + 1]          <- -3
      Spread[2 * m + 1]      <-  3
      Spread[length(Spread)] <- -1
      Spread <- matrix(Spread, nrow = m * 3 + 1, ncol = 1, byrow = TRUE)
      
      Var[[paste0("DF", m)]]           <- matrix(nrow = n, ncol = 1)
      rownames(Var[[paste0("DF", m)]]) <- EGTypeNames(OR_Contracts, Type)
      colnames(Var[[paste0("DF", m)]]) <- paste0("DF", m, "_Vars")
      
      for(i in 1:n){
        Var[[paste0("DF", m)]][i, 1] <- t(Spread) %*% OR_Cov[(i:(i + m * 3)), (i:(i + m * 3))] %*% Spread
      }
      
    }else if('DC' %in% substr(Type, 1, 2) && length(which(c(1:12) %in% substr(Type, 3, TL))) > 0){
      
      m <- as.numeric(substr(Type, 3, TL))
      n <- nrow(OR_Cov) - m * 4
      
      if(n < 1){stop(paste0("Not enough contracts in OR_Cov for ", Type))}
      
      Spread <- rep(0, m * 4 + 1)
      Spread[c(1, length(Spread))] <- c(1, -1)
      Spread[c(m + 1, m * 3 + 1)]  <- c(-2, 2)
      Spread <- matrix(Spread, nrow = m * 4 + 1, ncol = 1, byrow = TRUE)
      
      Var[[paste0("DC", m)]]           <- matrix(nrow = n, ncol = 1)
      rownames(Var[[paste0("DC", m)]]) <- EGTypeNames(OR_Contracts, Type)
      colnames(Var[[paste0("DC", m)]]) <- paste0("DC", m, "_Vars")
      
      for(i in 1:n){
        Var[[paste0("DC", m)]][i, 1] <- t(Spread) %*% OR_Cov[(i:(i + m * 4)), (i:(i + m * 4))] %*% Spread
      }
      
    }else if('DG' %in% substr(Type, 1, 2) && length(which(c(1:12) %in% substr(Type, 3, TL))) > 0){
      
      m <- as.numeric(substr(Type, 3, TL))
      n <- nrow(OR_Cov) - m * 5
      
      if(n < 1){stop(paste0("Not enough contracts in OR_Cov for ", Type))}
      
      Spread <- rep(0, m * 5 + 1)
      Spread[c(1, length(Spread))] <- c(1, -1)
      Spread[c(2*m+1, 3*m + 1)]    <- c(-5, 5)
      Spread <- matrix(Spread, nrow = m * 5 + 1, ncol = 1, byrow = TRUE)
      
      Var[[paste0("DG", m)]]           <- matrix(nrow = n, ncol = 1)
      rownames(Var[[paste0("DG", m)]]) <- EGTypeNames(OR_Contracts, Type)
      colnames(Var[[paste0("DG", m)]]) <- paste0("DG", m, "_Vars")
      
      for(i in 1:n){
        Var[[paste0("DG", m)]][i, 1] <- t(Spread) %*% OR_Cov[(i:(i + m * 5)), (i:(i + m * 5))] %*% Spread
      }
      
    }else if('RT' %in% substr(Type, 1, 2) && length(which(c(1:12) %in% substr(Type, 3, TL))) > 0){
      
      m <- as.numeric(substr(Type, 3, TL))
      n <- nrow(OR_Cov) - m * 5
      
      if(n < 1){stop(paste0("Not enough contracts in OR_Cov for ", Type))}
      
      Spread <- rep(0, m * 5 + 1)
      Spread[c(1, length(Spread))] <- c( 3,-3)
      Spread[c(m+1, m*5 - m + 1)]      <- c(-5, 5)
      Spread <- matrix(Spread, nrow = m * 5 + 1, ncol = 1, byrow = TRUE)
      
      Var[[paste0("RT", m)]]           <- matrix(nrow = n, ncol = 1)
      rownames(Var[[paste0("RT", m)]]) <- EGTypeNames(OR_Contracts, Type)
      colnames(Var[[paste0("RT", m)]]) <- paste0("RT", m, "_Vars")
      
      for(i in 1:n){
        Var[[paste0("RT", m)]][i, 1] <- t(Spread) %*% OR_Cov[(i:(i + m * 5)), (i:(i + m * 5))] %*% Spread
      }
      
    }else if('DD' %in% substr(Type, 1, 2) && length(which(c(1:12) %in% substr(Type, 3, TL))) > 0){
      
      m <- as.numeric(substr(Type, 3, TL))
      n <- nrow(OR_Cov) - m * 4
      
      if(n < 1){stop(paste0("Not enough contracts in OR_Cov for ", Type))}
      
      Spread <- rep(0, m * 4 + 1)
      Spread[c(1, m+1, 2*m+1, 3*m+1, length(Spread))] <- c(1, -4, 6, -4, 1)
      Spread <- matrix(Spread, nrow = m * 4 + 1, ncol = 1, byrow = TRUE)
      
      Var[[paste0("DD", m)]]           <- matrix(nrow = n, ncol = 1)
      rownames(Var[[paste0("DD", m)]]) <- EGTypeNames(OR_Contracts, Type)
      colnames(Var[[paste0("DD", m)]]) <- paste0("DD", m, "_Vars")
      
      for(i in 1:n){
        Var[[paste0("DD", m)]][i, 1] <- t(Spread) %*% OR_Cov[(i:(i + m * 4)), (i:(i + m * 4))] %*% Spread
      }
      
    }else if('PK' %in% Type){
      
      n <- nrow(OR_Cov) - 3
      
      if(n < 1){stop(paste0("Not enough contracts in OR_Cov for ", Type))}
      
      Spread <- matrix(rep(1, 4), nrow = 4, ncol = 1, byrow = TRUE)
      
      Var[["PK"]] <- matrix(nrow = n, ncol = 1)
      rownames(Var[["PK"]]) <- EGTypeNames(OR_Contracts, Type)
      colnames(Var[["PK"]]) <- "PK_Vars"
      
      for(i in 1:n){
        Var[["PK"]][i, 1] <- t(Spread) %*% OR_Cov[(i:(i + 3)), (i:(i + 3))] %*% Spread
      }
      
    }else if('PS' %in% Type){
      
      n <- nrow(OR_Cov) - 7
      
      if(n < 1){stop(paste0("Not enough contracts in OR_Cov for ", Type))}
      
      Spread <- matrix( c(rep(1, 4), rep(-1, 4)), nrow = 8, ncol = 1, byrow = TRUE)
      
      Var[["PS"]] <- matrix(nrow = n, ncol = 1)
      rownames(Var[["PS"]]) <- EGTypeNames(OR_Contracts, Type)
      colnames(Var[["PS"]]) <- "PS_Vars"
      
      for(i in 1:n){
        Var[["PS"]][i, 1] <- t(Spread) %*% OR_Cov[(i:(i + 7)), (i:(i + 7))] %*% Spread
      }
      
    }else if('PB' %in% Type){
      
      n <- nrow(OR_Cov) - 11
      
      if(n < 1){stop(paste0("Not enough contracts in OR_Cov for ", Type))}
      
      Spread <- matrix( c(rep(1, 4), rep(-2, 4), rep(1, 4)), nrow = 12, ncol = 1, byrow = TRUE)
      
      Var[["PB"]] <- matrix(nrow = n, ncol = 1)
      rownames(Var[["PB"]]) <- EGTypeNames(OR_Contracts, Type)
      colnames(Var[["PB"]]) <- "PB_Vars"
      
      for(i in 1:n){
        Var[["PB"]][i, 1] <- t(Spread) %*% OR_Cov[(i:(i + 11)), (i:(i + 11))] %*% Spread
      }
      
    }else if('FB' %in% substr(Type, 1, TL - 1) && 
               length(which(c(2:5) %in% substr(Type, TL, TL))) > 0){
      
      m <- as.numeric(substr(Type, TL, TL))
      n <- nrow(OR_Cov) - m * 4 + 1
      
      if(n < 1){stop(paste0("Not enough contracts in OR_Cov for ", Type))}
      
      Spread <- matrix(rep(1, m * 4), nrow = m * 4, ncol = 1, byrow = TRUE)
      
      Var[[paste0("FB", m)]] <- matrix(nrow = n, ncol = 1)
      rownames(Var[[paste0("FB", m)]]) <- EGTypeNames(OR_Contracts, Type)
      colnames(Var[[paste0("FB", m)]]) <- paste0("FB", m, "_Vars")
      
      for(i in 1:n){
        Var[[paste0("FB", m)]][i, 1] <- t(Spread) %*% OR_Cov[(i:(i + m * 4 - 1)), (i:(i + m * 4 - 1))] %*% Spread
      }
      
    }else if(Type %in% c('TED', 'ICS')){
      Vars <- numeric()
      ICS_Options <- character()
      for(i in 1:(ncol(OR_Cov)-1)){ for(j in (i+1):ncol(OR_Cov)){ if(getContractAsset(colnames(OR_Cov)[i]) != getContractAsset(colnames(OR_Cov)[j])){ ICS_Options <- append(ICS_Options, paste0(colnames(OR_Cov)[i], ".", colnames(OR_Cov)[j])) } } }
      
      sp.names <- character()
      for(ics in ICS_Options){
        Legs <- unlist(strsplit(ics, split = ".", fixed = TRUE))
        sp.names <- append(sp.names, paste0(ifelse(getContractAsset(Legs[1]) %in% c('TU','FV','TY','US','AUL','ES'), getContractAsset(Legs[1]), Legs[1]), ".", ifelse(getContractAsset(Legs[2]) %in% c('TU','FV','TY','US','AUL','ES'), getContractAsset(Legs[2]), Legs[2])))
        
        test <- try(load(paste0('/data/synthetics/', last(sp.names), '/', last(sp.names), '_Daily_Betas.RData')), silent = TRUE)
        if(class(test)=="try-error"){
          sp.names <- sp.names[-length(sp.names)]
          next
        }
        
        beta <- get(paste0(last(sp.names), ".Betas"))
        beta <- round(c(1, -as.numeric(beta[which(as.Date(index(beta)) == as.Date(date)),'TLS'])), 4)
        
        Vars <- append(Vars, t(as.matrix(beta)) %*% OR_Cov[Legs, Legs] %*% as.matrix(beta))
      }
      
      Var[['ICS']] <- matrix(Vars, nrow = length(Vars), ncol = 1, dimnames = list(sp.names, 'ICS_Vars'))
      
    }else if(Type %in% c('OG')){
      Vars <- numeric()
      OG_Options <- character()
      for(i in 1:(ncol(OR_Cov)-1)){ for(j in (i+1):ncol(OR_Cov)){ if(getContractAsset(colnames(OR_Cov)[i]) == getContractAsset(colnames(OR_Cov)[j])){ OG_Options <- append(OG_Options, paste0(colnames(OR_Cov)[i], ".", colnames(OR_Cov)[j])) } } }
        
      sp.names <- character()
      for(og in OG_Options){
        Legs <- unlist(strsplit(og, split = ".", fixed = TRUE))
        sp.names <- append(sp.names, paste0(ifelse(getContractAsset(Legs[1]) %in% c('TU','FV','TY','US','AUL','ES'), getContractAsset(Legs[1]), Legs[1]), ".", ifelse(getContractAsset(Legs[2]) %in% c('TU','FV','TY','US','AUL','ES'), getContractAsset(Legs[2]), Legs[2])))
        
        test <- try(load(paste0('/data/synthetics/', last(sp.names), '/', last(sp.names), '_Daily_Betas.RData')), silent = TRUE)
        if(class(test)=="try-error"){
          sp.names <- sp.names[-length(sp.names)]
          next
        }
        
        beta <- get(paste0(last(sp.names), ".Betas"))
        beta <- round(c(1, -as.numeric(beta[which(as.Date(index(beta)) == as.Date(date)),'TLS'])), 4)
        
        Vars <- append(Vars, t(as.matrix(beta)) %*% OR_Cov[Legs, Legs] %*% as.matrix(beta))
      }
      
      Var[['OG']] <- matrix(Vars, nrow = length(Vars), ncol = 1, dimnames = list(sp.names, 'OG_Vars'))
      
    }else{
      stop(paste0(Type, " Doesn't Exist Yet in CalcVar!"))
    }
  }
  
  return(Var)
}