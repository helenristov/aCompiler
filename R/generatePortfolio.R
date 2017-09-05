
#' Generates a portfolio for a given list of assets  
#'
#'@param Asset a list of asset classes 
#'@param Type A list of contract types that you want to generate for each asset class
#'@param years A list containing the number of years for each asset class
#'@param rev A reverse boolean that indicates the direction in which you want to pull the contracts. The default is set to FALSE for forward looking. Set to TRUE for building a portfolio in the past.  
#'
#'@author Helena Ristov 
#'
#'@export
#'


generatePortfolio <- function(Asset, Type, years, rev=FALSE){
  portfolio <- list()
  
  for(asset in Asset){
    ## convert years to number of periods
    if(asset == 'USTF'){ 
      ORs <- c()
      for(type in Type[[asset]]){
        ORs <- c(ORs,getTopStep(type, Sys.Date()))
      }
      portfolio[[asset]][['OR']] <- ORs  
      
    }else{
      year <- years[[asset]]
      
      if(asset == 'GE.TR'){
        front <- getTopStep("GE", Sys.Date())
      }else if(asset == 'ZQ.GE'){
        front <- getTopStep("ZQ", Sys.Date())
      }else{
        front <- getTopStep(asset, Sys.Date())
      }
    
      
      if(getInfo(front)$iter == "Quarterly"){ p <- 4*year }else{ p <- 12*year }
      ## generate the string of outrights and return them only contained in Types
      x <- ContractGenerator(front, p + 1, rev)
      
      if(!(asset %in% c('GE.TR', 'ZQ.GE'))){
        if(rev == TRUE){ x <- rev(x) }
        assign(paste0(asset, ".OR"), x)
        
        portfolio[[asset]][["OR"]] <- eval(parse(text = paste0(asset, ".OR")))
        
        if('OR' %in% Type[[asset]]){
          Type[[asset]] <- Type[[asset]][-which(Type[[asset]] == 'OR')]
        }
      }
      
      for(type in Type[[asset]]){
        if(asset == 'GE.TR' | asset == 'ZQ.GE'){
          hedge <- unlist(strsplit(type, '.', fixed = TRUE))[[2]]
          portfolio[[asset]][[type]] <- paste0(x, ".", hedge)
        }else{
          y <- EGTypeNames(eval(parse(text = paste0(asset, ".OR"))), type)
          assign(paste0(asset, ".", type), y)
          portfolio[[asset]][[type]] <- eval(parse(text = paste0(asset, ".", type)))
        }
      }
    }
  }
  return(portfolio)
}
