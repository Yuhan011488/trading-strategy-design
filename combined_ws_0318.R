# pre-loading -------------------------------------------------------------
# clean up the whole environment
rm(list = ls())  # clean up R environment
gc()  # clean up R memory
# assign("last.warning", NULL, envir = baseenv())  # clean up warnings

# preloading ------------------
# pre-loading all helper variables/functions/backtester
source('D:/preloading.R')

# init params---------------------------------------
params <- list(lookback=100,  # watch window, default 100, means no trade at the first 100 days  
               series=1:10,  # contol which series we want to trade, default 1:10
               posSizes=c(974, 158, 20486, 0, 74.56, 0, 0, 112.7, 65, 38),  # control series viable to TMA
               long_holdPeriod=6,  # control maximum long holding period 
               short_holdPeriod=5,  # control maximum short holding period 
               TMA_MA1=5, TMA_MA2=30, TMA_MA3=100,  # control short/Mid/Long term signals for TMA
               LMV_EMA1=5, LMV_EMA2=10, LMV_EMA3=20, LMV_EMA4=30, LMV_EMA5=100,  # control short/Mid/Long term signals for LMV
               BBMR_SMA=30,  # control exit SMA for BBMR
               BBMR_up_sdParam=2.5, BBMR_dn_sdParam=2.5,  # control Upper/Lower bounds for BBMR
               BBTF_up_sdParam=2.5, BBTF_dn_sdParam=2.5,  # control Upper/Lower bounds for BBTF
               srategyExit_params=8,  # controls maximum acceptable failing times for combined strategy
               VaR_p=0.99, VaR_ret=-0.05,  # control extreme risk measurement for VaR
               ES_p=0.99, ES_ret=-0.05,  # control extreme risk measurement for ES
               trailingStops_long_param=0.08,  # control trailing history bound when Long Hold
               trailingStops_short_param=0.08,  # control trailing history bound when Short Hold
               Stopgain_long_param=-0.01,  # control short-term Drawdown level when Long Hold
               Stopgain_short_param=0.01,  # control short-term Drawdown level when Short Hold
               Martingale=2.34)  # control martingale multiplier


# Combined Strategy  -------------------------

getOrders <- function(store, newRowList, currentPos, info, params){
  
  # init store
  
  if (is.null(store)) {   #init store in the first day
    store <- initStore(newRowList,params$series)}
  else{
    store <- updateStore(store, newRowList, params$series)  #after day 1, update everyday data to store
  }
  
  # init market Orders & limit Orders12 & limit Prices12
  
  marketOrders <- allzero
  limitOrders1 <- allzero;limitPrices1 <- allzero
  limitOrders2 <- allzero;limitPrices2 <- allzero
  
  if (store$iter >= params$lookback){
    
    # A kind Reminder
    # *******************************************
    # store$iter day 0 = moneySpendStore Day 1
    # *******************************************
    
    # calculate Today's real money spend on each series
    moneySpendCalc(params, store)
          
    # market condition filter(tbc)
    # 1. if trigger anomoly(e.g. extreme low volume...) then we stop trading for at least 30 days(tbd)
    # 2. ...
    
    bbtf <- getOrders.bbtf(store, newRowList, currentPos, info, params)
    tma <- getOrders.tma(store, newRowList, currentPos, info, params)
    bbmr <- getOrders.bbmr(store, newRowList, currentPos, info, params)
    lmv <- getOrders.lmv(store, newRowList, currentPos, info, params)
    
    # order by Priority (derived from individual Strategy Performance)
    orders_4 <- list(bbtf, tma, bbmr, lmv)
    
    # decide which strategy should dominate
    
    for(i in params$series){  # looping series
      
      # main logic
      
      if(currentPos[i] == 0 & flagStore[store$iter,i] == 6){ 
        order_index <- 1
        # looping orders order by Priority
        for(s in orders_4){
          # if orders[s] decides to long or short any shares of stock using market/limit orders
          # then stop and output orders to combined strategy orders
          if(s$marketOrders[i] != 0){
            marketOrders[i] <- s$marketOrders[i]
            
            # store which strategy is dominated which series everyday, store in codes
            # 1:4 stands for (bbtf, tma, bbmr, lmv) seperately
            SOCStore[store$iter+1,i] <<- order_index  
            break
          }
          order_index <- order_index + 1
        }
      } 
      
      else if(currentPos[i] != 0){  # if someone is on charge
        # continue using decisions originated from the same strategy
        SOCStore[store$iter+1,i] <<- SOCStore[store$iter,i]
        marketOrders[i] <- orders_4[[SOCStore[store$iter+1,i]]]$marketOrders[i]
      }
    }
    
    # Wager Strategy
    
    Martingale(params,store)
    # reversed_Martingale(params,store)
    
    # Risk Control Strategy
    
    # Stop-gain Strategy
    marketOrders <- Drawdown_Stopgain(params, store, currentPos, marketOrders)
    
    # Stop-loss Strategy
    # Time Stops
    marketOrders <- time_Stops(params, store, currentPos, marketOrders)
    # Trailing Stops
    marketOrders <- trailing_Stops(params, store, currentPos, marketOrders)
    # VaR and ES Stops
    marketOrders <- VaR_ES_Stops(params, store, currentPos, marketOrders)
    
    # Strategy Exit Strategy
    Strategy_Exit(params, store)
  }
  
  # log everyday trade signals
  SignalLog(params, store, currentPos, marketOrders, limitOrders1, limitOrder2)
  
  # update market/limit orders everyday
  marketOrderStore[store$iter+1,] <<- marketOrders  # stores market Orders of strategy on each series everyday
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,limitPrices2=limitPrices2))
}

# main ----------

test <- backtest(dataList, getOrders, params, sMult=0.2)
pfolioPnL <- plotResults(dataList, test, plotType='ggplot2', titleString='Combined Strategy')

# **********************************
# performance measurement-----
final_perf <- perfCalc(test=test, pfolioPnL=pfolioPnL)
