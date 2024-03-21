# pre-loading -------------------------------------------------------------
# clean up the whole environment
# rm(list = ls())  # clean up R environment
# gc()  # clean up R memory
# assign("last.warning", NULL, envir = baseenv())  # clean up warnings

# pre-loading all helper variables/functions/backtester
# preloading ------------------

# loading backtester framework
source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R')
source('framework/utilities.R'); 

# install libs
install.packages("quantmod")
# install.packages("egcm")
install.packages("tseries")
install.packages("PerformanceAnalytics")
install.packages("fBasics")
install.packages("TTR")
install.packages("vars")
install.packages("tsDyn")
install.packages("corrplot")
install.packages("tidyverse")
install.packages("zoo")
install.packages("pracma")
install.packages("aTSA")
install.packages("forecast")
install.packages("gridExtra")
install.packages("ggplot2")
install.packages("grid")
install.packages("DEoptim")

# loading libs
library(quantmod)
# library(egcm)
library(tseries)
library(PerformanceAnalytics)
library(fBasics)
library(TTR)
library(vars)
library(tsDyn)
library(corrplot)
library(tidyverse)
library(zoo)
library(pracma)
library(aTSA)
library(forecast)
library(gridExtra)
library(ggplot2)
library(grid)
library(DEoptim)

# loading data
# part1 <- getData(directory="PART1")
# part1<- getData(directory="PART1")
# part2<- getData(directory="PART2")
# part3<- getData(directory="PART3")

# dataList <- part3

# inSample <- lapply(part2, function(x) x[1:500])
# outSample <- lapply(part2, function(x) x[501:1000])

# in-sample (includes 1000 days part1 + 500 days part 2)
# for(i in 1:10){
#   dataList[[i]]<- rbind(part1[[i]], inSample[[i]])
# }


# average price
# series 1:10
# 25, 300, 0.74, 23, 87, 5, 99, 95, 86, 440


# init params---------------------------------------
# params for combined strategy
# init params---------------------------------------
params <- list(lookback=100,  # watch window, default 100, means no trade at the first 100 days  
               series=1:10,  # contol which series we want to trade, default 1:10
               posSizes=c(974, 158, 20486, 0, 74.56, 0, 0, 112.7, 65, 38),  # control series positions using Kelly
               long_holdPeriod=6,  # control maximum long holding period 
               short_holdPeriod=5,  # control maximum short holding period 
               TMA_MA1=5, TMA_MA2=30, TMA_MA3=100,  # control short/Mid/Long term signals for TMA
               LMV_EMA1=5, LMV_EMA2=10, LMV_EMA3=20, LMV_EMA4=30, LMV_EMA5=100,  # control short/Mid/Long term signals for TMA
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


# Helper global variables------------

# some init globals

allzero  <- rep(0,10) # used for initializing vectors
maxRows <- 3100  # set maxRows as the number of rows in data

# # stores strategy decisions on each series everyday
# 1:long enter, 2:short enter, 3:long hold, 
# 4:short hold, 5:clear position, 6:nothing happen
flagStore <- matrix(0,nrow=3100,ncol=10)
SOCStore <- matrix(0,nrow=3100,ncol=10) # stores strategy on charge on each series, 1:5 for 5 strategies

# some global variables to store everyday orders and real money spent for each series

moneySpendStore <- matrix(0,nrow=3100,ncol=10) # stores real money spent of strategy on each series everyday
marketOrderStore <- matrix(0,nrow=3100,ncol=10) # stores market orders on each series everyday

# some global variables for wager strategy and risk strategies

Martingale_multiplier <- rep(1,10) # Martingale multiplier
Martingale_lastProfit <- vector(mode="numeric",length=10) # stores profit in the last trade for each series over time
revMartingale_lastProfit <- vector(mode="numeric",length=10) # stores profit in the last trade for each series over time
timeStops_count <- rep(0,10) # for Time Stops holding period counting
trailStops_count <- vector(mode="numeric",length=10) # stores number of days in trade
SE_lastProfit <- vector(mode="numeric",length=10) # stores profit in the last trade for each series over time for Strategy Exit
lastProfit <- vector(mode="numeric",length=10) # stores profit in the last trade for each series over time
lossTimes <- vector(mode="numeric",length=10) # stores loss times
strategyExit_multiplier <- rep(1,10) # Strategy Exit multiplier
Drawdown_lastProfit <- vector(mode="numeric",length=10) # stores profit in the last trade for each series over time for Drawdown Stop Gain
cum_pnl <- vector(mode="numeric",length=10)  # stores cumulative pnl

# *******************************************************************
# Helper functions------------

# **********************************
# Store OHLCV for feeding backtester

# Store close price
initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}

# store open price
initOpStore<- function(newRowList,series){
  opStore<- matrix(0,nrow=maxRows,ncol=length(series))
  return(opStore)
}
updateOpStore<- function(opStore,newRowList,series,iter){
  for(i in 1:length(series))
    opStore[iter,i]<- as.numeric(newRowList[[series[i]]]$Open)
  return(opStore)
}

# store high price
initHiStore<- function(newRowList,series){
  hiStore<- matrix(0,nrow=maxRows,ncol=length(series))
  return(hiStore)
}
updateHiStore<- function(hiStore,newRowList,series,iter){
  for(i in 1:length(series))
    hiStore[iter,i]<- as.numeric(newRowList[[series[i]]]$High)
  return(hiStore)
}

# store low price
initLoStore<- function(newRowList,series){
  loStore<- matrix(0,nrow=maxRows,ncol=length(series))
  return(loStore)
}

updateLoStore<- function(loStore,newRowList,series,iter){
  for(i in 1:length(series))
    loStore[iter,i]<- as.numeric(newRowList[[series[i]]]$Low)
  return(loStore)
}

# store volume
initVolStore<- function(newRowList,series){
  volStore<- matrix(0,nrow=maxRows,ncol=length(series))
  return(volStore)
}

updateVolStore<- function(volStore,newRowList,series,iter){
  for(i in 1:length(series))
    volStore[iter,i]<- as.numeric(newRowList[[series[i]]]$Volume)
  return(volStore)
}

# store full list
initFullstore<- function(newRowList,series){
  fullStore<- vector(mode="list", length=length(series))
  fullStore<- lapply(fullStore, function(x) x=xts(data.frame(Open=0,High=0,Low=0,Close=0,Volume=0), as.Date(0))[-1,])
  return(fullStore)
}

updateFullStore<- function(fullStore,newRowList, series, iter){
  for(i in 1:length(series)){
    NEW<- xts(matrix(as.numeric(newRowList[[series[i]]]),nrow=1),as.Date(index(newRowList[[series[i]]])))
    fullStore[[i]]<- rbind(fullStore[[i]], NEW)
  }
  return(fullStore)
}

# TOTAL DATA STORED IN STORE VARIABLE
initStore <- function(newRowList,series) {
  return(list(iter=0,
              cl=initClStore(newRowList,series), 
              op=initOpStore(newRowList,series), 
              hi=initHiStore(newRowList,series), 
              lo=initLoStore(newRowList,series), 
              vol=initVolStore(newRowList,series),
              full= initFullstore(newRowList,series)
  )
  )
}

updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$op <- updateOpStore(store$op,newRowList,series,store$iter) 
  store$hi <- updateHiStore(store$hi,newRowList,series,store$iter) 
  store$lo <- updateLoStore(store$lo,newRowList,series,store$iter) 
  store$vol <- updateVolStore(store$vol,newRowList,series,store$iter)
  store$full<- updateFullStore(store$full,newRowList,series,store$iter)
  return(store)
}

# *********************
# Calculates slippage as a multiple of the overnight gap
#
# @param prevClose: previous day's close
# @param curOpen: current day's open
# @param sMult: slipppage multiplier, a positive value corresponds to negative slippage 
# @return: Slippage value

slip1  <-  function(prevClose, curOpen, sMult) { 
  overnightGap  <- abs(prevClose-curOpen)
  return(sMult * overnightGap) 
}

# *********************
# Creates list containing prices for previous, current and next row
#
# @param prevRow: Previous rows prices
# @param curRow: Current rows prices
# @param nextRow: Next rows prices (default: NULL)
# @return: list of prices and current date for use with other methods

getPrices1 <- function(prevRow, curRow) {
  # these are the three prices we need
  # nextOp-curOp is the pnl for today
  # curOp-prevCl is used to compute slippage
  prevCl     <- as.numeric(prevRow[,4])
  curOp     <- as.numeric(curRow[,1])
  curHi     <- as.numeric(curRow[,2])
  curLo     <- as.numeric(curRow[,3])
  prices     <- list(prevCl = prevCl, 
                     curOp = curOp, 
                     curHi = curHi, 
                     curLo = curLo)
  return(prices)
}

# *******************************************************************
# Gives pnl (until next open) and newPos arising from a *single* limit order

# checkLimitOrder1 <- function(prices,limitOrder,limitPrice) {
# 
#   eps <- 0.0001 # used to (try) to prevent floating errors when comparing limit
#   newPos  <- 0
#   moneySpent <- 0
#   executionPrice <- 0
# 
#   # execute if:
#   # buy  limit order (-eps) price is above low; or
#   # sell limit order (+eps) price is below high
#   if (limitOrder > 0 & prices$curLo < (limitPrice-eps) || limitOrder < 0 & prices$curHi > (limitPrice+eps))  {
# 
#     executionPrice <- ifelse(limitOrder > 0,
#                              min(limitPrice,prices$curHi), # execute buy order at minimum of limit price and High
#                              max(limitPrice,prices$curLo)) # execute sell order at maxmim of limit price and Low
#     moneySpent <- executionPrice * limitOrder
#     newPos <- limitOrder
#   }
#   
#   return(list(executionPrice=executionPrice, moneySpent=moneySpent, newPos=newPos))
# }

# *******************************************************************
# real money spent calculation for each series everyday

moneySpendCalc <- function(params, store){
  if (store$iter > params$lookback){
    for (i in params$series){
      if (flagStore[store$iter,i] == 1 | 
          flagStore[store$iter,i] == 2 | 
          flagStore[store$iter,i] == 5) {
        
        slippage1 <- slip1(store$cl[store$iter-1,i], store$op[store$iter,i], 0.2)
        
        # Market Order Money Spent is the execution (+ or - depending on buy or sell) price PLUS incurred slippage
        marketOrderMoneySpend <- (marketOrderStore[store$iter,i] * store$op[store$iter,i]) + 
          (abs(marketOrderStore[store$iter,i]) * slippage1)
        
        # Fetch prices
        # Creates list containing prices for previous, current and next row
        # @param prevRow: Previous rows prices
        # @param curRow: Current rows prices
        # @param nextRow: Next rows prices (default: NULL)
        # @return: list of prices and current date for use with other methods
        prices <- getPrices1(store$full[[i]][store$iter-1], store$full[[i]][store$iter])
        
        # l1 <- checkLimitOrder1(prices,limitOrders1Store[store$iter,i],limitPrices1Store[store$iter,i])
        # l2 <- checkLimitOrder1(prices,limitOrders2Store[store$iter,i],limitPrices2Store[store$iter,i])
        
        moneySpendStore[store$iter+1,i] <<- marketOrderMoneySpend
        # + l1$moneySpent + l2$moneySpent
        
        # limitOrders1Store[store$iter+1,i] <- l1$newPos # stores limit Orders 1 of strategy on each series everyday
        # limitOrders2Store[store$iter+1,i] <- l2$newPos # stores limit Orders 2 of strategy on each series everyday
        # limitPrices1Store[store$iter+1,i] <- l1$executionPrice # stores limit Price 1 of strategy on each series everyday
        # limitPrices2Store[store$iter+1,i] <- l2$executionPrice # stores limit Price 2 of strategy on each series everyday
      }
    }
  } 
}

# **********************************
# Log Trigger Signal
# record combined strategy trading signals

SignalLog <- function(params, store, currentPos, marketOrders, limitOrders1, limitOrder2){
  
  for(i in params$series){
    
    # enter conditions
    
    # currentpos is empty
    if(currentPos[i] == 0){
      if(marketOrders[i] > 0){  # if today marketOrder or limitOrder > 0
        flagStore[store$iter+1,i] <<- 1  # long enter flag
      }
      else if(marketOrders[i] < 0){  # if today marketOrder < 0
        flagStore[store$iter+1,i] <<- 2  # short enter flag
      }
    }
    
    # hold conditions
    
    # when in long position
    if (marketOrders[i] == 0){
      if(currentPos[i] > 0){  
        flagStore[store$iter+1,i] <<- 3  # long hold flag
      }
      # when in short position
      else if (currentPos[i] < 0){  
        flagStore[store$iter+1,i] <<- 4  # short hold flag
      }
    }
    
    # exit conditions
    
    if (currentPos[i] != 0 & marketOrders[i] != 0){
      flagStore[store$iter+1,i] <<- 5  # exit flag
    }
    
    # no trading condition
    
    if (currentPos[i] == 0 & marketOrders[i] == 0){
      flagStore[store$iter+1,i] <<- 6  # no trading flag
      SOCStore[store$iter+1,i] <<- 0  
    }
  }
}

# **********************************
# Martingale Wager strategy

Martingale <- function(params, store){
  for (i in params$series){
    # store money spend for establish position
    if (flagStore[store$iter,i] == 1 |
        flagStore[store$iter,i] == 2){
      Martingale_lastProfit[i] <<- -moneySpendStore[store$iter+1, i]
    }
    # when clear position, calculate this trade whether lose or win,
    # when losing, lastProfit = 1 , else 0
    else if(flagStore[store$iter,i] == 5){
      if (Martingale_lastProfit[i] - moneySpendStore[store$iter+1, i] < 0){
        Martingale_lastProfit[i] <<- 1
      }else{
        Martingale_lastProfit[i] <<- 0
      }
    }
    # when clear position and lose, double the wager for the next time
    if (flagStore[store$iter,i] == 5 & Martingale_lastProfit[i] == 1){
      Martingale_multiplier[i] <<- Martingale_multiplier[i] * params$Martingale
    }
    # if win, just reset the wager to the initial amount
    else if (flagStore[store$iter,i] == 5 & Martingale_lastProfit[i] == 0){
      Martingale_multiplier[i] <<- 1
    }
  }
}

# **********************************
# Reversed Martingale Wager strategy

reversed_Martingale <- function(params, store){
  for (i in params$series){
    # store money spend for establish position
    if (flagStore[store$iter,i] == 1 |
        flagStore[store$iter,i] == 2){
      revMartingale_lastProfit[i] <<- -moneySpendStore[store$iter+1, i]
    }
    # when clear position, calculate pnl
    # when winning, lastProfit = 1 , else 0
    else if(flagStore[store$iter,i] == 5){
      if (revMartingale_lastProfit[i] - moneySpendStore[store$iter+1, i] > 0){
        revMartingale_lastProfit[i] <<- 1
      }else{
        revMartingale_lastProfit[i] <<- 0
      }
    }
    
    # when clear position and win, double the wager for the next time
    if (flagStore[store$iter,i] == 5 & revMartingale_lastProfit[i] == 1){
      Martingale_multiplier[i] <<- Martingale_multiplier[i] * params$Martingale
    }
    # if win, just reset the wager to the initial amount
    else if (flagStore[store$iter,i] == 5 & revMartingale_lastProfit[i] == 0){
      Martingale_multiplier[i] <<- 1
    }
  }
}

# **********************************
# Drawdown Stop-gain method

# The Drawdown Stop-gain method is a strategy that does not involve any Stop-gain strategy in a bull market,
# and then redeems when there is a relatively large short-term Drawdown

Drawdown_Stopgain <- function(params, store, currentPos, Orders){
  for (i in params$series){
    # store money spend at position establish moment
    if (flagStore[store$iter,i] == 1 |
        flagStore[store$iter,i] == 2){
      Drawdown_lastProfit[i] <<- moneySpendStore[store$iter+1, i]
    }
    
    else if (flagStore[store$iter,i] == 3
             & store$cl[store$iter, i] > Drawdown_lastProfit[i]/currentPos[i]){
      if ((store$cl[store$iter, i] - store$cl[store$iter-1, i]) / store$cl[store$iter-1, i] < params$Stopgain_long_param){
        Orders[i] = -currentPos[i]
      }
    }
    else if(flagStore[store$iter,i] == 4
            & Drawdown_lastProfit[i]/currentPos[i] > store$cl[store$iter, i]){
      if ((store$cl[store$iter, i] - store$cl[store$iter-1, i]) / store$cl[store$iter-1, i] > params$Stopgain_short_param){
        Orders[i] = -currentPos[i]
      }
    }
  }
  return(Orders)
}  

# **********************************
# Time stops

# # check if we have been in trade too long
#
# # we maintain that pos[i] is an integer
# # if pos[i] == 0 we were flat last period
# # if pos[i] >  0 we have been long  for count[i] periods
# # if pos[i] <  0 we have been short for count[i] periods
#
time_Stops <- function(params, store, currentPos, Orders){
  for (i in params$series){
    
    # long holding period calculation
    
    if (flagStore[store$iter, i] == 1) { # in long position today
      timeStops_count[i] <<- 1
    }
    else if (timeStops_count[i] > 0 & Orders[i] == 0){
      timeStops_count[i] <<- timeStops_count[i] + 1
    }
    
    # short holding period calculation
    
    if (flagStore[store$iter, i] == 2) { # in short position today
      timeStops_count[i] <<- -1
    }
    else if (timeStops_count[i] < 0 & Orders[i] == 0){
      timeStops_count[i] <<- timeStops_count[i] - 1
    }
    
    if(timeStops_count[i] == params$long_holdPeriod |
       timeStops_count[i] == -params$short_holdPeriod){  # reached holding period
      Orders[i] <- -currentPos[i] # don't stay short
      timeStops_count[i] <<- 0 # reset count to 0
    }
  }
  
  return(Orders)
}

# **********************************
# Trailing Stops

# if daily close price decrease over 8% compared with 
# Highest Close in holding period,
# and we are in long position, then exit

trailing_Stops <- function(params, store, currentPos, Orders){
  for (i in params$series){
    # store num of days in trade each time
    if (flagStore[store$iter,i] == 1 |
        flagStore[store$iter,i] == 2){
      trailStops_count[i] <<- 1
    }
    
    else if(flagStore[store$iter,i] == 3 |
            flagStore[store$iter,i] == 4){
      if (store$cl[store$iter,i] < max(store$cl[(store$iter-trailStops_count[i]):(store$iter-1), i]) * (1-params$trailingStops_long_param) &
          currentPos[i] > 0){
        Orders[i] <- -currentPos[i] # don't stay long
      }
      else if (store$cl[store$iter,i] > min(store$cl[(store$iter-trailStops_count[i]):(store$iter-1), i]) * (1+params$trailingStops_short_param) &
               currentPos[i] < 0){
        Orders[i] <- -currentPos[i] # don't stay short
      }
      
      trailStops_count[i] <<- trailStops_count[i] + 1
    }
  }
  return(Orders)
}

# **********************************
# VaR and ES risk control

# to prevent extreme cases happen
VaR_ES_Stops <- function(params, store, currentPos, Orders){
  for (i in params$series){
    # Calculate returns
    RETS<- diff(log(store$cl[1:store$iter,i]))
    
    if(VaR(RETS, p=params$VaR_p, method = "gaussian") < params$VaR_ret){
      Orders[i] <- -currentPos[i]
    }
    else if(ES(RETS, p=params$ES_p, method = "gaussian") < params$ES_ret){
      Orders[i] <- -currentPos[i]
    }
  }
  return(Orders)
}

# **********************************
# Strategy Exit Mechanism

Strategy_Exit <- function(params, store){
  for (i in params$series){
    # if strategyExit on series i already triggered then just pass
    if (strategyExit_multiplier[i] == 0){
      next
    }
    
    # store money spend when enter
    if (flagStore[store$iter,i] == 1 |
        flagStore[store$iter,i] == 2){
      SE_lastProfit[i] <<- -moneySpendStore[store$iter+1, i]
    }
    # when clear position, calculate this trade whether lose or win,
    # when losing, SE_lastProfit = 1 , else 0
    else if(flagStore[store$iter,i] == 5){
      cum_pnl[i] <<- cum_pnl[i] + (SE_lastProfit[i] - moneySpendStore[store$iter+1, i])
      
      if (SE_lastProfit[i] - moneySpendStore[store$iter+1, i] < 0){
        SE_lastProfit[i] <<- 1
      }else{
        SE_lastProfit[i] <<- 0
      }
    }
    
    if (flagStore[store$iter,i] == 5 & SE_lastProfit[i] == 1){
      lossTimes[i] <<- lossTimes[i] + 1
    }
    # if win, just reset the lossTimes to the initial 0
    else if (flagStore[store$iter,i] == 5 & SE_lastProfit[i] == 0){
      lossTimes[i] <<- 0
    }
    
    # stop trading particular series 
    # if strategy bet wrong params$srategyExit_params times
    if (lossTimes[i] >= params$srategyExit_params){
      strategyExit_multiplier[i] <<- 0
    }
    
    if (cum_pnl[i] < -50000){
      strategyExit_multiplier[i] <<- 0
    }
  }
}

# **********************************
# performance measurement
perfCalc <- function(test, pfolioPnL){
  
  lastProfit <- vector(mode="numeric",length=10)
  winTimes <- vector(mode="numeric",length=10)
  WinBalance <- vector(mode="numeric",length=10)
  loseTimes <- vector(mode="numeric",length=10)
  loseBalance <- vector(mode="numeric",length=10)
  tradeTimes <- vector(mode="numeric",length=10)
  gambitRate <- vector(mode="numeric",length=10)
  kelly <- vector(mode="numeric",length=10)
  winRate <- vector(mode="numeric",length=10)
  lossRate <- vector(mode="numeric",length=10)
  
  pnl <- as.double(pfolioPnL[["pfoliosPnL"]][nrow(pfolioPnL[["pfoliosPnL"]]),2])
  PDratio <- pfolioPnL[["fitAgg"]]
  activeness <- as.double(test[["k"]])
  
  for (d in 2:1000){
    for (i in 1:10){
      # store money spend for establish position
      if (flagStore[d-1,i] == 1 |
          flagStore[d-1,i] == 2){
        lastProfit[i] <- -moneySpendStore[d, i]
      }
      # when clear position, calculate this trade whether lose or win,
      # when losing, lastProfit = 1 , else 0
      else if(flagStore[d-1,i] == 5){
        if (lastProfit[i] - moneySpendStore[d, i] > 0){
          winTimes[i] <- winTimes[i] + 1
          WinBalance[i] <- WinBalance[i] + (lastProfit[i] - moneySpendStore[d, i])
        }else if (lastProfit[i] - moneySpendStore[d, i] < 0){
          loseTimes[i] <- loseTimes[i] + 1
          loseBalance[i] <- loseBalance[i] + (lastProfit[i] - moneySpendStore[d, i])
        }
        tradeTimes[i] <- tradeTimes[i] + 1
      }
    }
  }
  
  for (i in 1:10){
    winRate[i] <- winTimes[i]/tradeTimes[i]
    lossRate[i] <- 1-winRate[i]
    gambitRate[i] <- (winTimes[i]/WinBalance[i]) / (loseTimes[i]/loseBalance[i])
    kelly[i] <- ((gambitRate[i]+1)*winRate[i] - 1) / gambitRate[i]
  }
  
  avg_winRate <- mean(winRate, na.rm = TRUE)
  avg_lossRate <- mean(lossRate, na.rm = TRUE)
  avg_winTimes <- mean(winTimes, na.rm = TRUE)
  avg_loseTimes <- mean(loseTimes, na.rm = TRUE)
  avg_tradeTimes <- mean(tradeTimes, na.rm = TRUE)
  sum_winTimes <- sum(winTimes, na.rm = TRUE)
  sum_loseTimes <- sum(loseTimes, na.rm = TRUE)
  sum_tradeTimes <- sum(tradeTimes, na.rm = TRUE)
  sum_kelly <- sum(kelly, na.rm = TRUE)
  expected_profit <- pnl / sum_tradeTimes
  
  res <- list('pnl'=pnl, 
              'PDratio'=PDratio,
              'activeness'=activeness,
              'winTimes'=winTimes, 
              'loseTimes'=loseTimes, 
              'tradeTimes'=tradeTimes, 
              'WinBalance'=WinBalance,
              'loseBalance'=loseBalance,
              'winRate'=winRate,
              'lossRate'=lossRate,
              'gambitRate'=gambitRate, 
              'kelly'=kelly,
              'avg_winRate'=avg_winRate,
              'avg_lossRate'=avg_lossRate,
              'avg_winTimes'=avg_winTimes,
              'avg_loseTimes'=avg_loseTimes,
              'avg_tradeTimes'=avg_tradeTimes,
              'sum_winTimes'=sum_winTimes,
              'sum_loseTimes'=sum_loseTimes,
              'sum_tradeTimes'=sum_tradeTimes,
              'sum_kelly'=sum_kelly,
              'expected_profit'=expected_profit
  )
  return(res)
}


# ********************************************************************
# Trading Strategy---------------------------------------

# ************************************
# 1. BBands Trend-following

getOrders.bbtf <- function(store, newRowList, currentPos, info, params){
  
  # init today's enter positions on each series 
  pos <- allzero
  
  # default exit yesterday's overall positions
  marketOrders <- -currentPos
  
  # we need to wait until lookback to get the bbands signal
  if (store$iter >= params$lookback){
    for (i in params$series){
      
      # Calculate everyday bbands upper/lower bounds
      up <- last(BBands(store$cl[1:store$iter,i], sd=params$BBTF_up_sdParam)[,3])
      dn <- last(BBands(store$cl[1:store$iter,i], sd=params$BBTF_dn_sdParam)[,1])
      
      # enter conditions
      
      # long enter when cross the upper bound
      if(store$cl[store$iter, i] > up){ 
        pos[i] <- params$posSizes[i] * Martingale_multiplier[i] * strategyExit_multiplier[i]
      } 
      # short enter when cross the lower bound
      else if(store$cl[store$iter, i] < dn){
        pos[i] <- -params$posSizes[i] * Martingale_multiplier[i] * strategyExit_multiplier[i]
      }
      
      # hold conditions
      
      if(currentPos[i] > 0){  # when in long position
        # hold when still in the long trend
        if (store$cl[store$iter, i] > store$cl[store$iter-1, i]){
          pos[i] <- currentPos[i]
        }
      }
      else if (currentPos[i] < 0){  # when in short position
        # hold when still in the short trend
        if (store$cl[store$iter, i] < store$cl[store$iter-1, i]){
          pos[i] <- currentPos[i]
        }
      }
      
      # exit conditions
      
      if(currentPos[i] != 0 & pos[i] == 0){ 
        pos[i] <- 0
      }
    }
  }
  
  marketOrders <- marketOrders + pos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}

# ************************************
# 2. Triple moving average Strategy

getOrders.tma<- function(store, newRowList, currentPos, info, params){
  # init today's enter positions on each series 
  pos <- allzero
  
  # default exit yesterday's overall positions
  marketOrders <- -currentPos
  
  # we need to wait until day 101 so that we have ma100 signal
  if(store$iter > params$lookback){
    for(i in params$series){
      
      # everyday re-calculate today's 
      # short term ma e.g. MA5
      # mid term ma e.g. MA30
      # long term ma e.g. MA100
      sma1 <- last(SMA(store$op[1:store$iter,i], params$TMA_MA1))
      sma2 <- last(SMA(store$op[1:store$iter,i], params$TMA_MA2))
      sma3 <- last(SMA(store$op[1:store$iter,i], params$TMA_MA3))
      
      # enter conditions
      
      # long enter when ma5 > ma30 > ma100
      if((sma1 > sma2) &  (sma2 > sma3)){
        # pos[i] <- params$posSizes[i] * 1000000 / store$cl[store$iter,i]
        pos[i] <- params$posSizes[i] * Martingale_multiplier[i] * strategyExit_multiplier[i]
        # short enter when ma5 < ma30 < ma100
      }
      else if((sma1 < sma2) & (sma2 < sma3)){
        pos[i] <- -params$posSizes[i] * Martingale_multiplier[i] * strategyExit_multiplier[i]
      }
      
      # hold conditions
      
      # long hold when still in the long trend
      if(currentPos[i] > 0 & store$cl[store$iter, i] > sma1){  
        pos[i] <- currentPos[i]
      }
      # short hold when still in the short trend
      else if (currentPos[i] < 0 & store$cl[store$iter, i] < sma1){
        pos[i] <- currentPos[i]
      }      
      
      # exit conditions
      
      if(currentPos[i] != 0 & pos[i] == 0){ 
        pos[i] <- 0
      }
    }
  }
  marketOrders <- marketOrders + pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}

# ************************************
# 3. BBands Mean-reversion

getOrders.bbmr <- function(store, newRowList, currentPos, info, params){
  # init today's enter positions on each series 
  pos <- allzero
  
  # default exit yesterday's overall positions
  marketOrders <- -currentPos
  
  # we need to wait until lookback to get the bbands signal
  if (store$iter > params$lookback){
    for (i in params$series){
      
      # calculate everyday bbands upper/lower bounds
      up <- last(BBands(store$cl[1:store$iter,i], sd=params$BBMR_up_sdParam)[,3])
      dn <- last(BBands(store$cl[1:store$iter,i], sd=params$BBMR_dn_sdParam)[,1])
      
      # everyday re-calculate today's ma30
      sma <- last(SMA(store$op[1:store$iter,i], params$BBMR_SMA))
      
      # enter conditions
      
      # short enter when cross the upper bound
      if(store$cl[store$iter, i] > up){ 
        pos[i] <- -params$posSizes[i] * Martingale_multiplier[i] * strategyExit_multiplier[i]
      } 
      # long enter when cross the lower bound
      else if(store$cl[store$iter, i] < dn){
        pos[i] <- params$posSizes[i] * Martingale_multiplier[i] * strategyExit_multiplier[i]
      }
      
      # hold conditions
      
      if(currentPos[i] > 0){  # when in long position
        # long hold when not reach MA
        if (store$cl[store$iter, i] > sma){
          pos[i] <- currentPos[i]
        }
        
      }
      else if (currentPos[i] < 0){  # when in short position
        # short hold when not reach MA
        if (store$cl[store$iter, i] < sma){
          pos[i] <- currentPos[i]
        }
      }
      
      # exit conditions
      
      if(currentPos[i] != 0 & pos[i] == 0){  
        pos[i] <- 0
      }
    }
  }
  marketOrders <- marketOrders + pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}

# ************************************
# 4. Lawrence Macmillan Volatility Trading System

getOrders.lmv<- function(store, newRowList, currentPos, info, params){
  # init today's enter positions on each series 
  pos <- allzero
  
  # default exit yesterday's overall positions
  marketOrders <- -currentPos
  
  # we need to wait until day 101 so that we have ema100 signal
  if(store$iter > params$lookback){
    for(i in params$series){
      
      # everyday re-calculate today's ema at diff. terms
      # e.g. ema5/ema10/ema20/ema30/ema100
      ema1 <- last(EMA(store$cl[1:store$iter,i],params$LMV_EMA1))
      ema2 <- last(EMA(store$cl[1:store$iter,i],params$LMV_EMA2))
      ema3 <- last(EMA(store$cl[1:store$iter,i],params$LMV_EMA3))
      ema4 <- last(EMA(store$cl[1:store$iter,i],params$LMV_EMA4))
      ema5 <- last(EMA(store$cl[1:store$iter,i],params$LMV_EMA5))
      
      # everyday re-calculate today's std5/std10/std20/std30/std100
      std1 <- store$cl[store$iter,i] - ema1
      std2 <- store$cl[store$iter,i] - ema2
      std3 <- store$cl[store$iter,i] - ema3
      std4 <- store$cl[store$iter,i] - ema4
      std5 <- store$cl[store$iter,i] - ema5
      
      # enter conditions
      
      # long enter when the volatility is getting smaller and in the down trend
      if(std1<std2 &
         std2<std3 &
         std3<std4 &
         std4<std5 &
         std1 < 0){
        pos[i] <- params$posSizes[i] * strategyExit_multiplier[i]
      }
      
      # hold conditions
      
      if(pos[i] != 0 | std1 < 0){  
        pos[i] <- currentPos[i]
      }
      
      # exit conditions
      
      if(currentPos[i] != 0 & pos[i] == 0){ 
        pos[i] <- 0
      }
    }
  }
  marketOrders <- marketOrders + pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}

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
            # 1:5 stands for (mm, bbtf, tma, bbmr, lmv) seperately
            SOCStore[store$iter+1,i] <<- order_index  
            break
          }
          order_index <- order_index + 1
        }
      } 
      
      else if(currentPos[i] != 0){  # if someone is on charge
        # continue using decisions made by the same strategy
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
  
  # log everyday on trade signals
  SignalLog(params, store, currentPos, marketOrders, limitOrders1, limitOrder2)
  
  # update market/limit orders everyday
  marketOrderStore[store$iter+1,] <<- marketOrders  # stores market Orders of strategy on each series everyday
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,limitPrices2=limitPrices2))
}

# main ----------
test <- backtest(dataList, getOrders, params, sMult=0.2)
pfolioPnL <- plotResults(dataList, test, plotType='ggplot2',
                              titleString='Combined Strategy')

# **********************************
# performance measurement-----
final_perf <- perfCalc(test=test, pfolioPnL=pfolioPnL)
# warnings()

# table(SOCStore[,10])

# write.csv(flagStore, "D:/flagStore_part1.xls")
# write.csv(marketOrderStore, "D:/marketOrderStore_part1.xls")
# write.csv(moneySpendStore, "D:/moneySpendStore_part1.xls")
# write.csv(SOCStore, "D:/SOCStore_part1.xls")