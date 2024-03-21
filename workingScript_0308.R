# pre-loading -------------------------------------------------------------
# clean up the whole environment
rm(list = ls())
gc()
assign("last.warning", NULL, envir = baseenv())


source('D:/wenhe/preloading.R'); 

# init params
params.bb <- list(lookback=20, series=1:10, 
                  posSizes=rep(1,10), 
                  sdParam=2.5, holdPeriod=6, 
                  srategyExit_params=5,
                  VaR_p=0.99, VaR_ret=-0.05,
                  ES_p=0.99, ES_ret=-0.05,
                  trailingStops_long_param=0.08,
                  trailingStops_short_param=0.08,
                  Stopgain_long_param=-0.01,
                  Stopgain_short_param=0.01)

####################################################################################
# 6.2 Trend-following---------
# Go Short when price crosses below lower band line;
# Go Long when price crosses above upper band line;

# main strategy
getOrders.bb <- function(store, newRowList, currentPos, info, params){
  
  # init store
  if (is.null(store)) {   #init store in the first day
    store <- initStore(newRowList,params$series)}
  else{
    store <- updateStore(store, newRowList, params$series)  #after day 1, update everyday data to store
  }
  
  # A kind Reminder
  # *******************************************
  # store$iter day 0 = moneySpendStore Day 1
  # ***************************************
  
  moneySpendCalc(params, store) 
  # init today's enter positions on each series 
  pos <- allzero
  
  # default exit yesterday's overall positions
  marketOrders <- -currentPos
  
  # we need to wait until lookback to get the bbands signal
  if (store$iter >= params$lookback){
    for (i in params$series){
      
      # Calculate everyday bbands upper/lower bounds
      up <- last(BBands(store$cl[1:store$iter,i], sd=params$sdParam)[,3])
      dn <- last(BBands(store$cl[1:store$iter,i], sd=params$sdParam)[,1])
      
      # establish position condition
      
      # long enter when cross the upper bound
      if(store$cl[store$iter, i] > up){ 
        pos[i] <- params$posSizes[i] * Martingale_multiplier[i] * strategyExit_multiplier[i]
        flagStore[store$iter+1,i] <<- 1  # long enter flag
        
        # short enter when cross the lower bound
      } else if(store$cl[store$iter, i] < dn){
        pos[i] <- -params$posSizes[i] * Martingale_multiplier[i] * strategyExit_multiplier[i]
        flagStore[store$iter+1,i] <<- 2  # short enter flag
      }
      
      # hold position condition
      
      if(currentPos[i] > 0){  # when in long position
        # hold when still in the long trend
        if ((pos[i] > 0) | 
            (store$cl[store$iter, i] > store$cl[store$iter-1, i])){
          pos[i] <- currentPos[i]
          flagStore[store$iter+1,i] <<- 3  # long hold flag
        }
      }else if (currentPos[i] < 0){  # when in short position
        # hold when still in the short trend
        if ((pos[i] < 0) | 
            (store$cl[store$iter, i] < store$cl[store$iter-1, i])){
          pos[i] <- currentPos[i]
          flagStore[store$iter+1,i] <<- 4  # short hold flag
        }
      }
      
      # exit position condition
      if (currentPos[i] != 0 & pos[i] == 0){
        flagStore[store$iter+1,i] <<- 5  # exit flag
      }
      
      # no trading condition
      else if (currentPos[i] == 0 & pos[i] == 0){
        flagStore[store$iter+1,i] <<- 6  # no trading flag
      }
    }
    
    marketOrders <- marketOrders + pos
    
    # Wager Strategy
    # Martingale(params,store)
    # reversed_Martingale(params,store)
    
    # Stop-gain Method
    marketOrders <- Drawdown_Stopgain(params,store, currentPos, marketOrders)
    
    # Stop-loss method
    # Time Stops
    new_pos_store <- time_Stops(params,store, currentPos, marketOrders)
    marketOrders <- new_pos_store$Orders
    store <- new_pos_store$store
    # Trailing Stops
    marketOrders <- trailing_Stops(params,store, currentPos, marketOrders)
    # VaR and ES Stops
    marketOrders <- VaR_ES_Stops(params,store, currentPos, marketOrders)
    
    # Strategy Exit Mechanism
    Strategy_Exit(params, store)
  }
  
  # update market/limit orders everyday
  marketOrderStore[store$iter+1,] <<- marketOrders
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}

####################################################################################

# **********************************
# for debugging

# tryCatch(
#   {

# **********************************
# main
test.bb<- backtest(dataList, getOrders.bb, params.bb, sMult=0.2)
pfolioPnL.bb <- plotResults(dataList, test.bb, plotType='ggplot2',
                            titleString='BBTF')


# performance measurement

# lastProfit <- vector(mode="numeric",length=10)
# winTimes <- vector(mode="numeric",length=10)
# WinBalance <- vector(mode="numeric",length=10)
# loseTimes <- vector(mode="numeric",length=10)
# loseBalance <- vector(mode="numeric",length=10)
# tradeTimes <- vector(mode="numeric",length=10)
# gambitRate <- vector(mode="numeric",length=10)
# kelly <- vector(mode="numeric",length=10)
# winRate <- vector(mode="numeric",length=10)
#
# pnl.bb <- as.double(pfolioPnL.bb[["pfoliosPnL"]][nrow(pfolioPnL.bb[["pfoliosPnL"]]),2])
# PDratio.bb <- pfolioPnL.bb[["fitAgg"]]
#
# for (d in 2:1000){
#   for (i in 1:10){
#     # store money spend for establish position
#     if (flagStore[d-1,i] == 1 |
#         flagStore[d-1,i] == 2){
#       lastProfit[i] <- -moneySpendStore[d, i]
#     }
#     # when clear position, calculate this trade whether lose or win,
#     # when losing, lastProfit = 1 , else 0
#     else if(flagStore[d-1,i] == 5){
#       if (lastProfit[i] - moneySpendStore[d, i] > 0){
#         winTimes[i] <- winTimes[i] + 1
#         WinBalance[i] <- WinBalance[i] + (lastProfit[i] - moneySpendStore[d, i])
#       }else if (lastProfit[i] - moneySpendStore[d, i] < 0){
#         loseTimes[i] <- loseTimes[i] + 1
#         loseBalance[i] <- loseBalance[i] + (lastProfit[i] - moneySpendStore[d, i])
#       }
#       tradeTimes[i] <- tradeTimes[i] + 1
#     }
#   }
# }
#
# for (i in 1:10){
#   winRate[i] <- winTimes[i]/tradeTimes[i]
#   gambitRate[i] <- (winTimes[i]/WinBalance[i]) / (loseTimes[i]/loseBalance[i])
#   kelly[i] <- ((gambitRate[i]+1)*winRate[i] - 1) / gambitRate[i]
# =function(e){
#   cat('i:',i,'\n')
#   cat('iter:',store$iter,'\n')
#   quit()
# }
# )
