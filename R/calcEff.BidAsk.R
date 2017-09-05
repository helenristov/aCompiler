#' Calculates Effective Bid Ask Prices
#'
#'@param market.data An MDR market data set.
#'@param createQty Start lean value to determine lean factor for edge calc.
#'@param cancelQty End lean value to determine lean factor for edge calc.
#'
#'@author Nicholas Dregne
#'
#'@export
#'
#'
## Measure used to calc WMP
calcEff.BidAsk <- function(market.data, createQty, cancelQty){
  
  market.data <- merge(market.data, market.data[,'AQ1'],  market.data[,'AQ1'] + market.data[,'AQ2'],  market.data[,'AQ1'] + market.data[,'AQ2'] + market.data[,'AQ3'],
                                    market.data[,'AQ1'] + market.data[,'AQ2'] + market.data[,'AQ3'] + market.data[,'AQ4'],  market.data[,'AQ1'] + market.data[,'AQ2'] + market.data[,'AQ3'] + market.data[,'AQ4'] + market.data[,'AQ5'],
                                    market.data[,'BQ1'],  market.data[,'BQ1'] + market.data[,'BQ2'],  market.data[,'BQ1'] + market.data[,'BQ2'] + market.data[,'BQ3'],
                                    market.data[,'BQ1'] + market.data[,'BQ2'] + market.data[,'BQ3'] + market.data[,'BQ4'],  market.data[,'BQ1'] + market.data[,'BQ2'] + market.data[,'BQ3'] + market.data[,'BQ4'] + market.data[,'BQ5'])
  colnames(market.data)[33:42] <- c(paste0('TAQ', c(1:5)), paste0('TBQ', c(1:5)))
  
  effs <- as.xts(apply(market.data[,c(paste0('BP', c(1:5)), paste0('TBQ', c(1:5)))], 1, function(x){ x[min(which(x[6:10] >= createQty))] } ), order.by = index(market.data))
  effs <- merge(effs, as.xts(apply(market.data[,c(paste0('BP', c(1:5)), paste0('TBQ', c(1:5)))], 1, function(x){ x[min(which(x[6:10] >= cancelQty))] } ), order.by = index(market.data)))
  
  effs <- merge(effs, as.xts(apply(market.data[,c(paste0('AP', c(1:5)), paste0('TAQ', c(1:5)))], 1, function(x){ x[min(which(x[6:10] >= cancelQty))] } ), order.by = index(market.data)))
  effs <- merge(effs, as.xts(apply(market.data[,c(paste0('AP', c(1:5)), paste0('TAQ', c(1:5)))], 1, function(x){ x[min(which(x[6:10] >= createQty))] } ), order.by = index(market.data)))
  
  effs <- merge(effs, market.data[,c('BP1','AP1')])
  effs <- cbind(cbind(effs, rep(NA, nrow(effs))), rep(NA, nrow(effs)))
  colnames(effs) <- c('Create.Eff.Bid', 'Cancel.Eff.Bid', 'Cancel.Eff.Ask', 'Create.Eff.Ask', 'BP1', 'AP1', 'EffBid', 'EffAsk')
  
  effs[1,c('EffBid', 'EffAsk')] <- effs[1,c('Create.Eff.Bid', 'Create.Eff.Ask')]
  for(i in 2:nrow(effs)){
    effs[i,'EffBid'] <- min(max(effs[i-1,'EffBid'], effs[i,'Create.Eff.Bid']), effs[i,'Cancel.Eff.Bid'])
    effs[i,'EffAsk'] <- max(min(effs[i-1,'EffAsk'], effs[i,'Create.Eff.Ask']), effs[i,'Cancel.Eff.Ask'])
  }
  
  effs <- as.xts(as.matrix(effs), order.by = as.POSIXct(index(effs)))
  
  return(effs)
}