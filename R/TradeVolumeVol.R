#'
#' Adjust Volatility based on Liquidity (Daily Traded Volume)
#'
#' Calculates Volatility Controllling for Potential Limited Liquidity.
#'
#' Compares recent daily traded volume to a minimally-accepted daily volume and adjusts volatility if violated.
#'
#'@param OR_Contracts List of outright contracts to liquidity test.  Derive other types from OR_Contracts vector.
#'@param StartTime Beginning date of liquidity analysis.
#'@param EndTime Ending date of liquidity analysis.
#'@param Types Types of combinations to examine for liquidity.
#'@param MinTrdVolume Minimum amount of liquidity necessary to prevent a liquidity risk adjustment to the regular volatility measurement.
#'
#'@author Nicholas Dregne
#'
#'@export

TradeVolumeVol <- function (OR_Contracts, StartTime, EndTime, Types, MinTrdVolume, ID = 'X.RIC', DataSource = 'TRS'){
  # directory <- "/data/tick/"
  # 
  #      KnownTypes <- c('OR','PK','PS','FB2','FB3','FB4','FB5','DF'
  #                      'CS1' ,'CS2' ,'CS3' ,'CS4' ,'CS5' ,'CS6' ,'CS7' ,'CS8' ,'CS9' ,'CS10','CS11','CS12',
  #                      'CS13','CS14','CS15','CS16','CS17','CS18','CS19','CS20','CS21','CS22','CS23','CS24',
  #                      'FL1' ,'FL2' ,'FL3' ,'FL4' ,'FL5' ,'FL6' ,'FL7' ,'FL8' ,'FL9' ,'FL10','FL11','FL12')
  
  Root      <- substr(OR_Contracts[1], 1, nchar(OR_Contracts[1]) - 2)
  RL        <- nchar(Root)
  NumOfDays <- length(GetWeekDays(StartTime, EndTime)) - ifelse(as.POSIXlt(StartTime)$hour > 12, 1, 0)
  
  VolAdj <- list()
  
  for(Type in Types){
    StringNum <- which(Type == Types)
    TL <- nchar(Type)
    
    if('OR' %in%  Type){
      
      Contracts <- OR_Contracts
      names(Contracts) <- OR_Contracts
      
      ContractData <- lapply(Contracts, function(x){ Pull.Algo.Data('Volume', x, StartTime, EndTime) })
      # if(DataSource == 'MDR'){
      #   ContractData   <- Pull.MDR.Data(Contracts, StartTime, EndTime, TOB = TRUE)
      # }else{
      #   ContractData   <- Pull.TRS.Data(Contracts, StartTime, EndTime, incr = 0, ID = ID)
      # }
      
      Avg.Daily.Vols <- unlist(lapply(Contracts, function(x) {sum(ContractData[[x]][,paste0(x,".Volume.1")], na.rm = TRUE)/NumOfDays}))
      
      VolAdj[[Type]] <- as.matrix(sapply(Avg.Daily.Vols, function(x) {min(x, MinTrdVolume[StringNum])/MinTrdVolume[StringNum]}))
      rownames(VolAdj[[Type]]) <- Contracts
      colnames(VolAdj[[Type]]) <- 'OR_Vols'
      
    }else if('CS1' %in% Type){
      
      n <- length(OR_Contracts) - 1
      Contracts <- paste0(OR_Contracts[-(n+1)], ".", substr(OR_Contracts[-1], RL + 1, RL + 2))
      names(Contracts) <- Contracts
      
      ContractData <- lapply(Contracts, function(x){ Pull.Algo.Data('Volume', x, StartTime, EndTime) })
      # if(DataSource == 'MDR'){
      #   ContractData   <- Pull.MDR.Data(Contracts, StartTime, EndTime, TOB = TRUE)
      # }else{
      #   ContractData   <- Pull.TRS.Data(Contracts, StartTime, EndTime, incr = 0, ID = ID)
      # }
      
      Avg.Daily.Vols <- unlist(lapply(Contracts, function(x) {sum(ContractData[[x]][,paste0(x,".Volume.1")], na.rm = TRUE)/NumOfDays}))
      
      VolAdj[[Type]] <- as.matrix(sapply(Avg.Daily.Vols, function(x) {min(x, MinTrdVolume[StringNum])/MinTrdVolume[StringNum]}))
      rownames(VolAdj[[Type]]) <- Contracts
      colnames(VolAdj[[Type]]) <- 'CS1_Vols'
            
    }else if('CS' %in% substr(Type, 1, 2) && length(which(c(2:24) %in% substr(Type, 3, TL))) > 0){
      
      m <- as.numeric(substr(Type, 3, TL))
      n <- length(OR_Contracts) - m
      
      Contracts <- paste0(OR_Contracts[-c((n+1):(n+m))], ".", substr(OR_Contracts[-c(1:m)], RL + 1, RL + 2))
      names(Contracts) <- Contracts
      
      if(MinTrdVolume[StringNum] >= 1){
        ContractData <- lapply(Contracts, function(x){ Pull.Algo.Data('Volume', x, StartTime, EndTime) })
        
        # if(DataSource == 'MDR'){
        #   ContractData   <- Pull.MDR.Data(Contracts, StartTime, EndTime, TOB = TRUE)
        # }else{
        #   ContractData   <- Pull.TRS.Data(Contracts, StartTime, EndTime, incr = 0, ID = ID)
        # }
        
        Avg.Daily.Vols <- unlist(lapply(Contracts, function(x) {sum(ContractData[[x]][,paste0(x,".Volume.1")], na.rm = TRUE)/NumOfDays}))
        
        VolAdj[[Type]] <- as.matrix(sapply(Avg.Daily.Vols, function(x) {min(x, MinTrdVolume[StringNum])/MinTrdVolume[StringNum]}))
        
      }else{
        VolAdj[[Type]] <- na.trim(as.matrix(SMA(VolAdj[['CS1']], m))) * ifelse(MinTrdVolume[StringNum] <= 0, 1, MinTrdVolume[StringNum])
      }
      
      rownames(VolAdj[[Type]]) <- Contracts
      colnames(VolAdj[[Type]]) <- paste0(Type, '_Vols')  
      
    }else if('FL' %in% substr(Type, 1, 2) && length(which(c(1:12) %in% substr(Type, 3, TL))) > 0){
      
      m <- as.numeric(substr(Type, 3, TL))
      n <- length(OR_Contracts) - m * 2
      
      Contracts <- paste0(Root, "BF", substr(OR_Contracts[1:n]            , RL + 1, RL + 2), ".", 
                          substr(OR_Contracts[(1+m):(n+m)]    , RL + 1, RL + 2), ".", 
                          substr(OR_Contracts[(1+m*2):(n+m*2)], RL + 1, RL + 2))
      names(Contracts) <- Contracts
      
      if(MinTrdVolume[StringNum] >= 1){
        ContractData <- lapply(Contracts, function(x){ Pull.Algo.Data('Volume', x, StartTime, EndTime) })
        # if(DataSource == 'MDR'){
        #   ContractData   <- Pull.MDR.Data(Contracts, StartTime, EndTime, TOB = TRUE)
        # }else{
        #   ContractData   <- Pull.TRS.Data(Contracts, StartTime, EndTime, incr = 0, ID = ID)
        # }
        
        Avg.Daily.Vols <- unlist(lapply(Contracts, function(x) {sum(ContractData[[x]][,paste0(x,".Volume.1")], na.rm = TRUE)/NumOfDays}))
        
        VolAdj[[Type]] <- as.matrix(sapply(Avg.Daily.Vols, function(x) {min(x, MinTrdVolume[StringNum])/MinTrdVolume[StringNum]}))
        
      }else{
        VolAdj[[Type]] <- na.trim(as.matrix(SMA(VolAdj[['CS1']], m*2))) * ifelse(MinTrdVolume[StringNum] <= 0, 1, MinTrdVolume[StringNum])
      }
      
      rownames(VolAdj[[Type]]) <- Contracts
      colnames(VolAdj[[Type]]) <- paste0(Type, '_Vols')  
      
    }else if('PK' %in% Type){
      
      n <- length(OR_Contracts) - 3
      
      Contracts <- paste0(Root, ":PK 01Y  ", substr(OR_Contracts[1:n], RL + 1, RL + 2))
      VolAdj[[Type]] <- na.trim(as.matrix(SMA(VolAdj[['OR']], 4))) * ifelse(MinTrdVolume[StringNum] <= 0, 1, MinTrdVolume[StringNum])
      
      rownames(VolAdj[[Type]]) <- Contracts
      colnames(VolAdj[[Type]]) <- paste0(Type, '_Vols')  
      
    }else if('PS' %in% Type){
      
      n <- length(OR_Contracts) - 7
      
      Contracts <- paste0(Root, ":PS  ", substr(OR_Contracts[1:n], RL + 1, RL + 2), "-", substr(OR_Contracts[5:(n+4)], RL + 1, RL + 2))
      VolAdj[[Type]] <- na.trim(as.matrix(SMA(VolAdj[['CS4']], 4))) * ifelse(MinTrdVolume[StringNum] <= 0, 1, MinTrdVolume[StringNum])
      
      rownames(VolAdj[[Type]]) <- Contracts
      colnames(VolAdj[[Type]]) <- paste0(Type, '_Vols')  
      
    }else if('PB' %in% Type){
      
      n <- length(OR_Contracts) - 11
      
      Contracts <- paste0(Root, ":PB  ", substr(OR_Contracts[1:n], RL + 1, RL + 2), "-", substr(OR_Contracts[5:(n+4)], RL + 1, RL + 2), "-", substr(OR_Contracts[9:(n+8)], RL + 1, RL + 2))
      VolAdj[[Type]] <- na.trim(as.matrix(SMA(VolAdj[['CS4']], 8))) * ifelse(MinTrdVolume[StringNum] <= 0, 1, MinTrdVolume[StringNum])
      
      rownames(VolAdj[[Type]]) <- Contracts
      colnames(VolAdj[[Type]]) <- paste0(Type, '_Vols')  
      
    }else if('FB' %in% substr(Type, 1, TL - 1) && length(which(c(2:5) %in% substr(Type, TL, TL))) > 0){
      
      m <- as.numeric(substr(Type, TL, TL))
      n <- length(OR_Contracts) - m * 4 + 1
      
      Contracts <- paste0(Root, ":FB 0", m, "Y ", substr(OR_Contracts[1:n], RL + 1, RL + 2))                          
      VolAdj[[Type]] <- na.trim(as.matrix(SMA(VolAdj[['OR']], m*4))) * ifelse(MinTrdVolume[StringNum] <= 0, 1, MinTrdVolume[StringNum])
      
      rownames(VolAdj[[Type]]) <- Contracts
      colnames(VolAdj[[Type]]) <- paste0(Type, '_Vols')  
      
    }else if(substr(Type, 1, 2) %in% c('DF', 'CN', 'DC')){
      
      m <- as.numeric(substr(Type, 3, TL))
      if(substr(Type, 1, 2) == 'DC'){ n <- length(OR_Contracts) - 4 * m }else{ n <- length(OR_Contracts) - 3 * m }
      
      if(substr(Type, 1, 2) == 'DC'){
        Contracts <- paste0(Root, ":", substr(Type, 1, 2), " ", substr(OR_Contracts[1:n    ], RL + 1, RL + 2),
                                                                substr(OR_Contracts[2:(n+1)], RL + 1, RL + 2),
                                                                substr(OR_Contracts[4:(n+3)], RL + 1, RL + 2),
                                                                substr(OR_Contracts[5:(n+4)], RL + 1, RL + 2))
      }else{
        Contracts <- paste0(Root, ":", substr(Type, 1, 2), " ", substr(OR_Contracts[1:n    ], RL + 1, RL + 2),
                                                                substr(OR_Contracts[2:(n+1)], RL + 1, RL + 2),
                                                                substr(OR_Contracts[3:(n+2)], RL + 1, RL + 2),
                                                                substr(OR_Contracts[4:(n+3)], RL + 1, RL + 2))  
      }
      
      VolAdj[[Type]] <- na.trim(as.matrix(SMA(VolAdj[['CS1']], ifelse(substr(Type, 1, 2) == 'DC', 4, 3)))) * ifelse(MinTrdVolume[StringNum] <= 0, 1, MinTrdVolume[StringNum])
      
      rownames(VolAdj[[Type]]) <- Contracts
      colnames(VolAdj[[Type]]) <- paste0(Type, '_Vols')  
      
    }else{
      stop(paste0(Type, " Doesn't Exist Yet!"))
    }
    
  }
  
  return(VolAdj)
}