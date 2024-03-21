# pre-loading -------------------------------------------------------------
# clean up the whole environment
rm(list = ls())

# loading backtester framework
source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R')
source('framework/utilities.R'); 

# loading libraries
library(quantmod)
library(egcm)
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

# loading data ------------------------------------------------------------
# dataList <- getData(directory="PART1")
# part1<- getData(directory="PART1")
dataList<- getData(directory="PART2")
# part3<- getData(directory="PART3")

inSample <- lapply(dataList, function(x) x[1:500])
outSample <- lapply(dataList, function(x) x[501:1000])

# dataList <- inSample

# Preliminary implementations of strategy  --------------------------------

# Store OHLCV for feeding backtester --------------------------------------

# ============Store close price========================
initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
# ============store open price===================================
initOpStore<- function(newRowList,series){
  opStore<- matrix(0,nrow=maxRows,ncol=length(series))
  return(opStore)
}
updateOpStore<- function(opStore,newRowList,series,iter){
  for(i in 1:length(series))
    opStore[iter,i]<- as.numeric(newRowList[[series[i]]]$Open)
  return(opStore)
}
# ============store high price===================================
initHiStore<- function(newRowList,series){
  hiStore<- matrix(0,nrow=maxRows,ncol=length(series))
  return(hiStore)
}
updateHiStore<- function(hiStore,newRowList,series,iter){
  for(i in 1:length(series))
    hiStore[iter,i]<- as.numeric(newRowList[[series[i]]]$High)
  return(hiStore)
}
# ============store low price===================================
initLoStore<- function(newRowList,series){
  loStore<- matrix(0,nrow=maxRows,ncol=length(series))
  return(loStore)
}

updateLoStore<- function(loStore,newRowList,series,iter){
  for(i in 1:length(series))
    loStore[iter,i]<- as.numeric(newRowList[[series[i]]]$Low)
  return(loStore)
}

# ============store volume======================================
initVolStore<- function(newRowList,series){
  volStore<- matrix(0,nrow=maxRows,ncol=length(series))
  return(volStore)
}

updateVolStore<- function(volStore,newRowList,series,iter){
  for(i in 1:length(series))
    volStore[iter,i]<- as.numeric(newRowList[[series[i]]]$Volume)
  return(volStore)
}

#============store full list====================================
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

# ============TOTAL DATA STORED IN STORE VARIABLE=========================================
initStore <- function(newRowList,series) {
  return(list(iter=0,
              cl=initClStore(newRowList,series), 
              op=initOpStore(newRowList,series), 
              hi=initHiStore(newRowList,series), 
              lo=initLoStore(newRowList,series), 
              vol=initVolStore(newRowList,series),
              full= initFullstore(newRowList,series),
              count <- vector(mode="numeric",length=10), # stores # of days in trade
              moneySpend <- matrix(0,nrow=maxRows,ncol=length(series)),
              flag <- matrix(0,nrow=maxRows,ncol=length(series))))
}

updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$op <- updateOpStore(store$op,newRowList,series,store$iter) 
  store$hi <- updateHiStore(store$hi,newRowList,series,store$iter) 
  store$lo <- updateLoStore(store$lo,newRowList,series,store$iter) 
  store$vol <- updateVolStore(store$vol,newRowList,series,store$iter)
  store$full<- updateFullStore(store$full,newRowList,series,store$iter)
  store$count <- store$count
  store$moneySpend <- store$moneySpend
  store$flag <- store$flag
  return(store)
}

# used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)
maxRows <- 3100

# we maintain that pos[i] is an integer
# if pos[i] == 0 we were flat last period
# if pos[i] >  0 we have been long  for store$count[i] periods
# if pos[i] <  0 we have been short for store$count[i] periods

# 1. Triple moving average Strategy ---------------------------------------
# long signal: short > medium > long
# short signal: short < medium < long
# we got 4 types of MA can be used: VWAP/EMA/SMA/Rolling Medians
# we will use 7 days MA to represent short-term trend,
# 30 days MA to medium-term trend, 90 days to long-term trend 
# which respectively stands for weekly/monthly/seasonally trend in nature
# because we noticed there are no holiday in our dataset so we just use 7 days instead of 5

# init params
params.tma<- list(lookback=100, series=1:10, posSizes=rep(1,10), holdPeriod=6) # tbd

# main strategy
getOrders.tma<- function(store, newRowList, currentPos, info, params){
  # used for initializing vectors
  allzero  <- rep(0,length(newRowList))
  # 
  # # init store
  # if (is.null(store)) {
  #   store <- initStore(newRowList,params$series)}else{
  #     store <- updateStore(store, newRowList, params$series) 
  #   }
  
  # init today's enter positions on each series 
  pos <- allzero
  
  # default exit yesterday's overall positions
  marketOrders <- -currentPos
  
  # we need to wait until day 101 so that we have ma100 signal
  if(store$iter > params$lookback){
    for(i in params$series){
      # everyday re-calculate today's ma5/ma30/ma100
      sma5 <- last(SMA(store$op[1:store$iter,i],5))
      sma30 <- last(SMA(store$op[1:store$iter,i],30))
      sma100 <- last(SMA(store$op[1:store$iter,i],100))
      
      # enter conditions
      
      # long enter when ma5 > ma30 > ma100
      if((sma5 > sma30) &  (sma30 > sma100)){
        # pos[i] <- params$posSizes[i] * 1000000 / store$cl[store$iter,i]
        pos[i] <- params$posSizes[i]
      # short enter when ma5 < ma30 < ma100
      }else if((sma5 < sma30) & (sma30 < sma100)){
        pos[i] <- -params$posSizes[i]
      }
      
      # exit conditions
      
      if(currentPos[i] > 0){  # when in long position
        # long exit when new close is no longer the new highest close
        if (store$cl[store$iter, i] < store$cl[store$iter-1, i]){
          pos[params$series[i]] <- 0
        }

      }else if (currentPos[i] < 0){  # when in short position
        # short exit when new close is no longer the new lowest close
        if (store$cl[store$iter, i] > store$cl[store$iter-1, i]){
          pos[params$series[i]] <- 0
        }
      }
      # 
      # # check if we have been in trade too long
      # 
      # # we maintain that pos[i] is an integer
      # # if pos[i] == 0 we were flat last period
      # # if pos[i] >  0 we have been long  for store$count[i] periods
      # # if pos[i] <  0 we have been short for store$count[i] periods
      # 
      # if (pos[params$series[i]] > 0) {# long signal today
      #   if (store$count[i] < 0) # last time we were short
      #     store$count[i] == 1
      #   else if (store$count[i] == params$holdPeriod) { # reached holding period
      #     pos[params$series[i]] <- -currentPos[i] # don't stay long
      #     store$count[i] <- 0 # reset count to 0
      #   }
      #   else # 0 <= store$count[i] != (should be <) params$holdPeriod
      #     store$count[i] <- store$count[i] + 1
      # }
      # 
      # else if (pos[params$series[i]] < 0) {# short signal today
      #   if (store$count[i] > 0) # last time we were long
      #     store$count[i] == -1
      #   else if (store$count[i] == -params$holdPeriod) { # reached holding period
      #     pos[params$series[i]] <- -currentPos[i] # don't stay short
      #     store$count[i] <- 0 # reset count to 0
      #   }
      #   else # 0 >= store$count[i] != (should be >) -params$holdPeriod
      #     store$count[i] <- store$count[i] - 1
      # }
      # else{
      #   store$count[i] <- 0 # reset count to 0
      # }
    }
  }
  marketOrders <- marketOrders + pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}

test.tma<- backtest(dataList, getOrders.tma, params.tma, sMult=0.2)
pfolioPnL.tma <- plotResults(dataList, test.tma, plotType='ggplot2',
                             titleString='Triple moving average Strategy ')

# rolling window method
window <- 200
intl <- 50
i = 0
resagg <- NULL
outdta <- dataList

while((i+window) < nrow(outdta[[1]])){
  ddta <- lapply(outdta, function(x) x[(i+1):(i+window),])
  test <- backtest(ddta, getOrders.tma, params.tma, sMult=0.2)
  pfolioPnL <- plotResults(ddta, test, plotType='ggplot2')
  perf <- pfolioPnL$fitAgg
  resagg <- c(resagg, perf)
  i <- i+intl
}
plot(resagg,type="l")
title(c('pd ratio of cumulative pnl over period'))

# 2. Relative strength Strategy -------------------------------------------
# Asset classes/ industry sectors have different relationships with the business cycle
# Pick 3 series with strongest 12 month momentum (pricechange/SO/RSI/SMI/MA on pricechange) to go long
# Pick 3 series with weakest 12 month momentum (pricechange/SO/RSI/SMI/MA on pricechange) to go short
# into our portfolio and weight them equally (tbd). 
# Rebalance once every month.

# init params
params.rs <- list(lookback=365, series=1:10, posSizes=1) # tbd

# main strategy
# 2. Relative strength Strategy -------------------------------------------

getOrders.rs<- function(store, newRowList, currentPos, info, params){
  # init today's enter positions on each series 
  pos <- allzero
  
  # # init store
  if (is.null(store)) {
    store <- initStore(newRowList,params$series)}else{
      store <- updateStore(store, newRowList, params$series)
  }
  
  # default exit yesterday's overall positions
  marketOrders <- allzero
  
  # we need to wait until day 366 so that we have open price 1 yr earlier
  # and the day must be 30 days after last trading day
  # (the first trading day should be day 366)
  if((store$iter > params$lookback) & ((store$iter-365)  %% 30 == 1)){
    # default exit yesterday's overall positions
    marketOrders <- -currentPos
    
    # init mtm/max 3 mtm series/min 3 mtm series
    mtm <- allzero
    max3mtm_series <- allzero
    min3mtm_series <- allzero
    
    for(i in params$series1){
      # re-calculate today's pricechange = today's open - open 1 year earlier
      # we use change in percentage to represent pricechange 
      # to eliminate the differences in price measurement  
      mtm[i] <- (store$op[store$iter,i]-store$op[store$iter-365,i]) / 
        (store$op[store$iter-365,i])
    }
    
    # the best 3 momentum series 
    max3mtm_series <- order(mtm,decreasing=TRUE)[1:3]
    # the worst 3 momentum series
    min3mtm_series <- order(mtm,decreasing=FALSE)[1:3]
    
    for(i in params$series){
      # long enter
      if(i %in% max3mtm_series){
        # pos[i] <- params$posSizes*1000000/store$cl[store$iter,i]
        pos[params$series[i]] <- params$posSizes
      }
      # short enter
      else if(i %in% min3mtm_series){
        pos[params$series[i]] <- - params$posSizes
      }
    }
  }
  marketOrders <- marketOrders + pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}

test.rs<- backtest(dataList, getOrders.rs, params.rs, sMult=0.2)
pfolioPnL.rs <- plotResults(dataList, test.rs, plotType='ggplot2', 
                            titleString='Relative strength Strategy')

# rolling window method
window <- 200
intl <- 50
i = 0
resagg <- NULL
outdta <- dataList

while((i+window) < nrow(outdta[[1]])){
  ddta <- lapply(outdta, function(x) x[(i+1):(i+window),])
  test <- backtest(ddta, getOrders.rs, params.rs, sMult=0.2)
  pfolioPnL <- plotResults(ddta, test, plotType='ggplot2')
  perf <- pfolioPnL$fitAgg
  resagg <- c(resagg, perf)
  i <- i+intl
}
plot(resagg,type="l")
title(c('pd ratio of cumulative pnl over period'))

# 3. Market Making Strategy -----------------------------------------------
# One of the most important types of high-frequency trading strategies is 
# Market Making, i.e., making profits by earning bid-ask spreads
# But in our case, we want to exploit the market by setting a proper 
# spread range so that we can betting on both sides by limit orders
# Buy low and sell high to make profit

# init params
params.mm <- list(lookback=0, series=1:10, 
                  posSizes=rep(1,10), spread=0.08, holdPeriod=6) # tbd

# main strategy
getOrders.mm<- function(store, newRowList, currentPos, info, params){
  # used for initializing vectors
  allzero  <- rep(0,length(newRowList))
  
  # if our positions reach 2 times betting level then exit yesterday's overall positions
  marketOrders<- ifelse(abs(currentPos)>2*params$posSizes, -currentPos, 0)
  
  # init store
  if (is.null(store)) {
    store <- initStore(newRowList,params$series)}else{
      store <- updateStore(store, newRowList, params$series) 
    }
  
  # init today's enter positions on each series 
  pos <- allzero
  
  # init limitPrices1/limitPrices2/limitOrders1/limitOrders2
  limitPrices1<- allzero; limitOrders1<- allzero
  limitPrices2<- allzero; limitOrders2<- allzero
  
  if(store$iter > params$lookback){
    for(i in params$series){
      # market making - betting on both sides
      # limitOrders1[i]<- params$posSizes[i]*1000000/store$cl[store$iter,i]
      limitOrders1[i]<- params$posSizes[i]
      limitPrices1[i]<- (store$hi[store$iter,i]+store$lo[store$iter,i])/2-params$spread*store$cl[store$iter,i]
      limitOrders2[i]<- -params$posSizes[i]
      limitPrices2[i]<- (store$hi[store$iter,i]+store$lo[store$iter,i])/2+params$spread*store$cl[store$iter,i]
      # 
      # # check if we have been in trade too long
      # 
      # # we maintain that pos[i] is an integer
      # # if pos[i] == 0 we were flat last period
      # # if pos[i] >  0 we have been long  for store$count[i] periods
      # # if pos[i] <  0 we have been short for store$count[i] periods
      # 
      # if (pos[params$series[i]] > 0) {# long signal today
      #   if (store$count[i] < 0) # last time we were short
      #     store$count[i] == 1
      #   else if (store$count[i] == params$holdPeriod) { # reached holding period
      #     pos[params$series[i]] <- -currentPos[i] # don't stay long
      #     store$count[i] <- 0 # reset count to 0
      #   }
      #   else # 0 <= store$count[i] != (should be <) params$holdPeriod
      #     store$count[i] <- store$count[i] + 1
      # }
      # 
      # else if (pos[params$series[i]] < 0) {# short signal today
      #   if (store$count[i] > 0) # last time we were long
      #     store$count[i] == -1
      #   else if (store$count[i] == -params$holdPeriod) { # reached holding period
      #     pos[params$series[i]] <- -currentPos[i] # don't stay short
      #     store$count[i] <- 0 # reset count to 0
      #   }
      #   else # 0 >= store$count[i] != (should be >) -params$holdPeriod
      #     store$count[i] <- store$count[i] - 1
      # }
      # else{
      #   store$count[i] <- 0 # reset count to 0
      # }
    }
  }
  marketOrders <- marketOrders + pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,limitPrices2=limitPrices2))
} 

test.mm<- backtest(dataList, getOrders.mm, params.mm, sMult=0.2)
pfolioPnL.mm <- plotResults(dataList, test.mm, plotType='ggplot2',
                            titleString='Market Making Strategy')

# rolling window method
window <- 200
intl <- 50
i = 0
resagg <- NULL
outdta <- dataList

while((i+window) < nrow(outdta[[1]])){
  ddta <- lapply(outdta, function(x) x[(i+1):(i+window),])
  test <- backtest(ddta, getOrders.mm, params.mm, sMult=0.2)
  pfolioPnL <- plotResults(ddta, test, plotType='ggplot2')
  perf <- pfolioPnL$fitAgg
  resagg <- c(resagg, perf)
  i <- i+intl
}
plot(resagg,type="l")
title(c('pd ratio of cumulative pnl over period'))

# 4. The Jump Trading Strategy --------------------------------------------
# (abandoned)
#
# The Jump Trading System is a psychological trading system that 
# measures sudden price changes due primarily to excessive emotional reactions.
# Its basic movement is in a downtrend where prices fluctuate 
# around the lower range of the box range for 5 to 10 days, 
# then open price sharply lower below the trend line with extreme selling sentiment, 
# and if it then rebounds to yesterday's lows, it indicates a reversal of market energy 
# and another bullish upward move is ready to take place. Systematic buying mechanical rules.
# 1. close price 4% below the five-day MA to ensure the signal occurs on a downtrend.
# 2. and open price is 1% below yesterday's low price.
# 3. Closing rally above yesterday's low price.

# init params
params.jt <- list(lookback=5, series=1:10, posSizes=rep(1,10), holdPeriod=6) # tbd

# main strategy
getOrders.jt<- function(store, newRowList, currentPos, info, params){
  # used for initializing vectors
  allzero  <- rep(0,length(newRowList))
  
  # init store
  if (is.null(store)) {
    store <- initStore(newRowList,params$series)}else{
      store <- updateStore(store, newRowList, params$series) 
    }
  
  # init today's enter positions on each series 
  pos <- allzero
  
  # default exit yesterday's overall positions
  marketOrders <- -currentPos
  
  # we need to wait until day 101 so that we have ema100 signal
  if(store$iter > params$lookback){
    for(i in params$series){
      # everyday re-calculate today's ema5
      ema5 <- last(EMA(store$cl[1:store$iter,i],5))
      
      # long enter
      if(((store$cl[store$iter,i] - ema5) / ema5 <= -0.04) &  
         ((store$op[store$iter,i] - store$lo[store$iter-1,i]) / store$lo[store$iter-1,i] <= -0.01) &
         (store$cl[store$iter,i] > store$lo[store$iter-1,i])){
        # pos[i] <- params$posSizes[i]*1000000 / store$cl[store$iter,i]
        pos[i] <- params$posSizes[i]
        
      # exit when direction turned again
      }else if(((currentPos[i] != 0) &
                (store$cl[store$iter,i] < store$cl[store$iter-1,i]))){
        pos[i] <- 0
        
      # hold when still in the trend
      }else if(((currentPos[i] != 0) &
                (store$cl[store$iter,i] >= store$cl[store$iter-1,i]))){
        pos[i] <- currentPos[i]
      }
      # 
      # # check if we have been in trade too long
      # 
      # # we maintain that pos[i] is an integer
      # # if pos[i] == 0 we were flat last period
      # # if pos[i] >  0 we have been long  for store$count[i] periods
      # # if pos[i] <  0 we have been short for store$count[i] periods
      # 
      # if (pos[params$series[i]] > 0) {# long signal today
      #   if (store$count[i] < 0) # last time we were short
      #     store$count[i] == 1
      #   else if (store$count[i] == params$holdPeriod) { # reached holding period
      #     pos[params$series[i]] <- 0 # don't stay long
      #     store$count[i] <- 0 # reset count to 0
      #   }
      #   else # 0 <= store$count[i] != (should be <) params$holdPeriod
      #     store$count[i] <- store$count[i] + 1
      # }
      # 
      # else if (pos[params$series[i]] < 0) {# short signal today
      #   if (store$count[i] > 0) # last time we were long
      #     store$count[i] == -1
      #   else if (store$count[i] == -params$holdPeriod) { # reached holding period
      #     pos[params$series[i]] <- 0 # don't stay short
      #     store$count[i] <- 0 # reset count to 0
      #   }
      #   else # 0 >= store$count[i] != (should be >) -params$holdPeriod
      #     store$count[i] <- store$count[i] - 1
      # }
      # else{
      #   store$count[i] <- 0 # reset count to 0
      # }
    }
  }
  marketOrders <- marketOrders + pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}

test.jt<- backtest(dataList, getOrders.jt, params.jt, sMult=0.2)
pfolioPnL.jt <- plotResults(dataList, test.jt, plotType='ggplot2',
                            titleString='The Jump Trading Strategy')

# 5. Lawrence Macmillan Volatility Trading System -------------------------
# Volatility is the rate at which stock prices change and can be calculated 
# using the standard deviation formula and by comparing historical 
# volatility over different lengths of time, such as 10, 20 and 50 days, 
# and 100 days. Systematic buying mechanics rules.
# 1. historical volatility is short aligned, 
# i.e. the volatility is getting smaller, suggesting the peace before the storm.
# 2. calculate historical volatility at 5, 10, 20, 30 and 100 days and find its standard deviation.
# 3. AC and AO indicators fall for 5 consecutive days.

# init params
params.lmv <- list(lookback=100, series=1:10, 
                   posSizes=rep(1,10), holdPeriod=6) # tbd

# main strategy
getOrders.lmv<- function(store, newRowList, currentPos, info, params){
  # used for initializing vectors
  allzero  <- rep(0,length(newRowList))
  
  # init store
  if (is.null(store)) {
    store <- initStore(newRowList,params$series)}else{
      store <- updateStore(store, newRowList, params$series) 
    }
  
  # init today's enter positions on each series 
  pos <- allzero
  
  # default exit yesterday's overall positions
  marketOrders <- -currentPos
  
  # we need to wait until day 101 so that we have ema100 signal
  if(store$iter > params$lookback){
    for(i in params$series){
      # everyday re-calculate today's ema5/ema10/ema20/ema30/ema100
      ema5 <- last(EMA(store$cl[1:store$iter,i],5))
      ema10 <- last(EMA(store$cl[1:store$iter,i],10))
      ema20 <- last(EMA(store$cl[1:store$iter,i],20))
      ema30 <- last(EMA(store$cl[1:store$iter,i],30))
      ema100 <- last(EMA(store$cl[1:store$iter,i],100))
      
      # everyday re-calculate today's std5/std10/std20/std30/std100
      std5 <- store$cl[store$iter,i] - ema5
      std10 <- store$cl[store$iter,i] - ema10
      std20 <- store$cl[store$iter,i] - ema20
      std30 <- store$cl[store$iter,i] - ema30
      std100 <- store$cl[store$iter,i] - ema100
      
      # long enter when the volatility is getting smaller and in the down trend
      if((std5<std10) &
         (std10<std20) &
         (std20<std30) &
         (std30<std100) &
         (std5 < 0)){
        pos[i] <- params$posSizes[i]} 
          
      # long exit when direction turned up
      if((currentPos[i] > 0)
                & (ema5 >= 0)){
        pos[i] <- 0
      }
      
      # # check if we have been in trade too long
      # 
      # # we maintain that pos[i] is an integer
      # # if pos[i] == 0 we were flat last period
      # # if pos[i] >  0 we have been long  for store$count[i] periods
      # # if pos[i] <  0 we have been short for store$count[i] periods
      # 
      # if (pos[params$series[i]] > 0) {# long signal today
      #   if (store$count[i] < 0) # last time we were short
      #     store$count[i] == 1
      #   else if (store$count[i] == params$holdPeriod) { # reached holding period
      #     pos[params$series[i]] <- 0 # don't stay long
      #     store$count[i] <- 0 # reset count to 0
      #   }
      #   else # 0 <= store$count[i] != (should be <) params$holdPeriod
      #     store$count[i] <- store$count[i] + 1
      # }
      # 
      # else if (pos[params$series[i]] < 0) {# short signal today
      #   if (store$count[i] > 0) # last time we were long
      #     store$count[i] == -1
      #   else if (store$count[i] == -params$holdPeriod) { # reached holding period
      #     pos[params$series[i]] <- 0 # don't stay short
      #     store$count[i] <- 0 # reset count to 0
      #   }
      #   else # 0 >= store$count[i] != (should be >) -params$holdPeriod
      #     store$count[i] <- store$count[i] - 1
      # }
      # else{
      #   store$count[i] <- 0 # reset count to 0
      # }
    }
  }
  marketOrders <- marketOrders + pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}
test.lmv<- backtest(dataList, getOrders.lmv, params.lmv, sMult=0.2)
pfolioPnL.lmv <- plotResults(dataList, test.lmv, plotType='ggplot2',
                            titleString='Lawrence Macmillan Volatility Trading System')

# rolling window method
window <- 200
intl <- 50
i = 0
resagg <- NULL
outdta <- dataList

while((i+window) < nrow(outdta[[1]])){
  ddta <- lapply(outdta, function(x) x[(i+1):(i+window),])
  test <- backtest(ddta, getOrders.lmv, params.lmv, sMult=0.2)
  pfolioPnL <- plotResults(ddta, test, plotType='ggplot2')
  perf <- pfolioPnL$fitAgg
  resagg <- c(resagg, perf)
  i <- i+intl
}
plot(resagg,type="l")
title(c('pd ratio of cumulative pnl over period'))


# 6. BBands based Strategy ------------------------------------------------
# we know several ideas about how to use Bbands to make money from 226
# so naturally, we might want to test if we can develop these ideas in 
# more depth.

# 6.1 Mean-reversion
# Go Long when price crosses below lower band line;
# exit when price crosses above moving average
# Go Short when price crosses above upper band line;
# exit when price crosses below moving average

# init params
params.bb<- list(lookback=30, series=1:10, 
                 posSizes=rep(1,10), 
                 sdParam=2.5, holdPeriod=6) # tbd

# main strategy
getOrders.bb<- function(store, newRowList, currentPos, info, params){
  # used for initializing vectors
  allzero  <- rep(0,length(newRowList))
  
  # init store
  if (is.null(store)) {
    store <- initStore(newRowList,params$series)}else{
      store <- updateStore(store, newRowList, params$series) 
    }
  
  # init today's enter positions on each series 
  pos <- allzero
  
  # default exit yesterday's overall positions
  marketOrders <- -currentPos
  
  # we need to wait until lookback to get the bbands signal
  if (store$iter > params$lookback){
    for (i in params$series){
      # calculate everyday bbands upper/lower bounds
      up <- last(BBands(store$cl[1:store$iter,i], sd=params$sdParam)[,3])
      dn <- last(BBands(store$cl[1:store$iter,i], sd=params$sdParam)[,1])
      
      # everyday re-calculate today's ma30
      sma30 <- last(SMA(store$op[1:store$iter,i],30))
      
      # long enter when cross the upper bound
      if(store$cl[store$iter, i] > up){ 
        pos[params$series[i]] <- -params$posSizes[i]
        
        # short enter when cross the lower bound
      } else if(store$cl[store$iter, i] < dn){
        pos[params$series[i]] <- params$posSizes[i]
      }
      
      if(currentPos[i] > 0){  # when in long position
        # long hold when not reach MA
        if ((pos[params$series[i]] > 0) | 
            (store$cl[store$iter, i] > sma30)){
          pos[params$series[i]] <- currentPos[i]
          next
        }
        
      }else if (currentPos[i] < 0){  # when in short position
        # short hold when not reach MA
        if ((pos[params$series[i]] < 0) | 
            (store$cl[store$iter, i] < sma30)){
          pos[params$series[i]] <- currentPos[i]
          next
        }
      }
      # 
      # # check if we have been in trade too long
      # 
      # # we maintain that pos[i] is an integer
      # # if pos[i] == 0 we were flat last period
      # # if pos[i] >  0 we have been long  for store$count[i] periods
      # # if pos[i] <  0 we have been short for store$count[i] periods
      # 
      # if (pos[params$series[i]] > 0) {# long signal today
      #   if (store$count[i] < 0) # last time we were short
      #     store$count[i] == 1
      #   else if (store$count[i] == params$holdPeriod) { # reached holding period
      #     pos[params$series[i]] <- 0 # don't stay long
      #     store$count[i] <- 0 # reset count to 0
      #   }
      #   else # 0 <= store$count[i] != (should be <) params$holdPeriod
      #     store$count[i] <- store$count[i] + 1
      # }
      # 
      # else if (pos[params$series[i]] < 0) {# short signal today
      #   if (store$count[i] > 0) # last time we were long
      #     store$count[i] == -1
      #   else if (store$count[i] == -params$holdPeriod) { # reached holding period
      #     pos[params$series[i]] <- 0 # don't stay short
      #     store$count[i] <- 0 # reset count to 0
      #   }
      #   else # 0 >= store$count[i] != (should be >) -params$holdPeriod
      #     store$count[i] <- store$count[i] - 1
      # }
      # else{
      #   store$count[i] <- 0 # reset count to 0
      # }
    }
  }
  marketOrders <- marketOrders + pos
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}

test.bb<- backtest(dataList, getOrders.bb, params.bb, sMult=0.2)
pfolioPnL.bb <- plotResults(dataList, test.bb, plotType='ggplot2',
                            titleString='Bbands based Strategy-mr')


####################################################################################
# Calculates slippage as a multiple of the overnight gap
#
# @param prevClose: previous day's close
# @param curOpen: current day's open
# @param sMult: slipppage multiplier, a positive value corresponds to negative slippage 
# @return: Slippage value

slip  <-  function(prevClose, curOpen, sMult) { 
  overnightGap  <- abs(prevClose-curOpen)
  return(sMult * overnightGap) 
}

count <- vector(mode="numeric",length=10) # stores # of days in trade
moneySpend <- matrix(0,nrow=maxRows,ncol=10)
flag <- matrix(0,nrow=maxRows,ncol=length(series))


# 6.2 Trend-following
# Go Short when price crosses below lower band line;
# Go Long when price crosses above upper band line;

# init params
params.bb <- list(lookback=20, series=1:10, 
                 posSizes=rep(1,10), 
                 sdParam=2.5, holdPeriod=6) # tbd

# main strategy
getOrders.bb<- function(store, newRowList, currentPos, info, params){
  # used for initializing vectors
  allzero  <- rep(0,length(newRowList))
  
  # init store
  if (is.null(store)) {
    store <- initStore(newRowList,params$series)}else{
      store <- updateStore(store, newRowList, params$series) 
    }
  
  if (store$iter > params$lookback){
    for (i in params$series){
      
      cat('***')
      cat(store$iter)
      cat('***')
      cat(i)
      cat('***')
      cat(store$flag[store$iter,i])
      cat('***')
      cat("\n")
      
      # if (store$flag[store$iter-1,i]==1 | store$flag[store$iter-1,i]==3){
      # 
      #   # run from day 2, where oldPos would always be 0, until penultimate day
      #   slippage <- slip(store$cl[store$iter-1,i], store$op[store$iter,i], 0.2)
      #   
      #   # Money spent is the execution (+ or - depending on buy or sell) price PLUS incurred slippage
      #   mo <- (marketOrders * store$op[store$iter,i]) + (abs(marketOrders) * slippage)
      # 
      # # Fetch prices
      # # Creates list containing prices for previous, current and next row
      # # @param prevRow: Previous rows prices
      # # @param curRow: Current rows prices
      # # @param nextRow: Next rows prices (default: NULL)
      # # @return: list of prices and current date for use with other methods
      #   prices <- getPrices(rows$prev, rows$cur, rows$nxt)
      # 
      #   l1 <- checkLimitOrder(prices,limitOrders1[i],limitPrices1[i])
      #   l2 <- checkLimitOrder(prices,limitOrders2[i],limitPrices2[i]) 
      # 
      #   store$moneySpent[store$iter,i] <- mo + l1$moneySpent + l2$moneySpent
      # }
    }
  }
  
  
  # init today's enter positions on each series 
  pos <- allzero
  
  # default exit yesterday's overall positions
  marketOrders <- -currentPos
  
  # we need to wait until lookback to get the bbands signal
  if (store$iter > params$lookback){
    for (i in params$series){
      # flag <- allzero
      
      # calculate everyday bbands upper/lower bounds
      up <- last(BBands(store$cl[1:store$iter,i], sd=params$sdParam)[,3])
      dn <- last(BBands(store$cl[1:store$iter,i], sd=params$sdParam)[,1])
      
      # establish position condition
      # long enter when cross the upper bound
      if(store$cl[store$iter, i] > up){ 
        pos[params$series[i]] <- params$posSizes[i]
        store$flag[store$iter,i] <- 1  # establish position
        
      # short enter when cross the lower bound
      } else if(store$cl[store$iter, i] < dn){
        pos[params$series[i]] <- -params$posSizes[i]
        store$flag[store$iter,i] <- 1  # establish position
      }
      
      # hold position condition
      else if(currentPos[i] > 0){  # when in long position
        # hold when still in the long trend
        if ((pos[params$series[i]] > 0) | 
            (store$cl[store$iter, i] > store$cl[store$iter-1, i])){
          pos[params$series[i]] <- currentPos[i]
          store$flag[store$iter,i] <- 2  # hold position flag
        }
        
      }else if (currentPos[i] < 0){  # when in short position
        # hold when still in the short trend
        if ((pos[params$series[i]] < 0) | 
            (store$cl[store$iter, i] < store$cl[store$iter-1, i])){
          pos[params$series[i]] <- currentPos[i]
          store$flag[store$iter,i] <- 2  # hold position flag
        }
      }
      
      # clear position condition
      if (currentPos[i] != 0 & pos[params$series[i]] == 0){
        store$flag[store$iter,i] <- 3
      }
      # no trading condition
      else if (currentPos[i] == 0 & pos[params$series[i]] == 0){
        store$flag[store$iter,i] <- 4
      }
      
      # # check if we have been in trade too long
      # 
      # # we maintain that pos[i] is an integer
      # # if pos[i] == 0 we were flat last period
      # # if pos[i] >  0 we have been long  for store$count[i] periods
      # # if pos[i] <  0 we have been short for store$count[i] periods
      # 
      # if (pos[params$series[i]] > 0) {# long signal today
      #   if (store$count[i] < 0) # last time we were short
      #     store$count[i] == 1
      #   else if (store$count[i] == params$holdPeriod) { # reached holding period
      #     pos[params$series[i]] <- 0 # don't stay long
      #     store$count[i] <- 0 # reset count to 0
      #   }
      #   else # 0 <= store$count[i] != (should be <) params$holdPeriod
      #     store$count[i] <- store$count[i] + 1
      # }
      # 
      # else if (pos[params$series[i]] < 0) {# short signal today
      #   if (store$count[i] > 0) # last time we were long
      #     store$count[i] == -1
      #   else if (store$count[i] == -params$holdPeriod) { # reached holding period
      #     pos[params$series[i]] <- 0 # don't stay short
      #     store$count[i] <- 0 # reset count to 0
      #   }
      #   else # 0 >= store$count[i] != (should be >) -params$holdPeriod
      #     store$count[i] <- store$count[i] - 1
      # }
      # else{
      #   store$count[i] <- 0 # reset count to 0
      # }
    }
  }
  marketOrders <- marketOrders + pos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}

test.bb<- backtest(dataList, getOrders.bb, params.bb, sMult=0.2)
pfolioPnL.bb <- plotResults(dataList, test.bb, plotType='ggplot2',
                            titleString='Bbands based Strategy-tf')

# The Kelly Criterion (wip)
# f* = (b*p-q)/b
# f*: the best betting proportion
# b: gambit rate (expected profit/expected loss in one game)
# p: win rate
# q: loss rate
# 
# 
# wintimes <- NULL
# losetimes <- NULL
# winbalance <- NULL
# losebalance <- NULL
# entertimes <- NULL
# 
# for (j in 1:10){
#   daypassed <- NULL
#   for(i in 1:500){
#     if (i %in% daypassed){
#       
#       # long enter
#       if (test.bb$positionValuesList[[j]][i] > 0){
#         entertimes[j] <- entertimes[j] + 1
#         
#         # loop from day i onward to find the exit day
#         for(k in i:500){
#           daypassed <- append(daypassed, k)
#           
#           # exit day
#           if (test.bb$positionValuesList[[j]][k] = 0){
#             if (test.bb$positionValuesList[[j]][k-1]>test.bb$positionValuesList[[j]][i]){
#               wintimes[j] <- wintimes[j] + 1
#               winbalance[j] <- winbalance[j] + (test.bb$positionValuesList[[j]][k-1]-test.bb$positionValuesList[[j]][i])
#             }else if (test.bb$positionValuesList[[j]][k-1]<=test.bb$positionValuesList[[j]][i]){
#               losetimes[j] <- losetimes[j] + 1
#               losebalance[j] <- losebalance[j] + (test.bb$positionValuesList[[j]][k-1]-test.bb$positionValuesList[[j]][i])
#             }
#           }
#         }
#       }
#       
#       # short enter
#       else if (test.bb$positionValuesList[[j]][i] < 0){
#         entertimes[j] <- entertimes[j] + 1
#         
#         # loop from day i onward to find the exit day
#         for(k in i:500){
#           daypassed <- append(daypassed, k)
#           
#           # exit day
#           if (test.bb$positionValuesList[[j]][k] = 0){
#             if (test.bb$positionValuesList[[j]][k-1]>test.bb$positionValuesList[[j]][i]){
#               wintimes[j] <- wintimes[j] + 1
#               winbalance[j] <- winbalance[j] + (test.bb$positionValuesList[[j]][k-1]-test.bb$positionValuesList[[j]][i])
#             }else if (test.bb$positionValuesList[[j]][k-1]<=test.bb$positionValuesList[[j]][i]){
#               losetimes[j] <- losetimes[j] + 1
#               losebalance[j] <- losebalance[j] + (test.bb$positionValuesList[[j]][k-1]-test.bb$positionValuesList[[j]][i])
#             }
#           }
#         }
#       }
#       
#     }
#   }
# }


# rolling window method
window <- 200
intl <- 50
i = 0
resagg <- NULL
outdta <- part2
while((i+window) < nrow(outdta[[1]])){
  ddta <- lapply(outdta, function(x) x[(i+1):(i + window),])
  test <- backtest(ddta, getOrders.bb, params.bb, sMult=0.2)
  pfolioPnL <- plotResults(ddta, test, plotType='ggplot2')
  perf <- pfolioPnL$fitAgg
  resagg<- c(resagg, perf)
  i <- i+intl
}
plot(resagg,type="l")
title(c('pd ratio of cumulative pnl over period'))

# parameter optimization --------------------------------------------------
# (wip)

# grid search with cross validation ---------------------------------------

# insample series
# the first 500 rows of data in datalist
insample<- lapply(dataList, function(x) x[1:500,])

# RSI trading parameters Optimization
# (n & lev)
sMult <- 0.2 # slippage multiplier
nSeq<- c(12,14,16,18,20,22,24,26,28)
levSeq  <- c(10,15,20,25,30,35,40)
paramsList<- list(nSeq, levSeq)
numberComb <- prod(sapply(paramsList,length))

resultsMatrixRSI <- matrix(nrow=numberComb,ncol=3)
colnames(resultsMatrixRSI) <- c("n","lev","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (n in nSeq) {
  for (lev in levSeq) {
    INparams.rsi<- list(lookback=33, series=1:10,posSizes=rep(0.01,10),n=n,lev=lev ) # need to be justified
    results <- backtest(insample, getOrders.RSI, INparams.rsi, sMult)
    pfolioPnL <- plotResults(insample,results)
    resultsMatrixRSI[count,] <- c(n,lev,pfolioPnL$fitAgg)
    pfolioPnLList[[count]]<- pfolioPnL
    cat("Just completed",count,"out of",numberComb,"\n")
    print(resultsMatrixRSI[count,])
    count <- count + 1
  }
}
INn<- resultsMatrixRSI[order(resultsMatrixRSI[,"PD Ratio"]),][nrow(resultsMatrixRSI),1]
INlev<- resultsMatrixRSI[order(resultsMatrixRSI[,"PD Ratio"]),][nrow(resultsMatrixRSI),2]

# RSI Series Optimization
INparams.rsi<- list(lookback=50, series=1:10,posSizes=rep(0.01,10),n=14,lev=25 )
insampleTest.rsi<-  backtest(insample, getOrders.RSI,INparams.rsi, sMult=0.2)
insamplePfolioPnL.rsi<- plotResults(insample,insampleTest.rsi,plotType='ggplot2')


# in out sample test ------------------------------------------------------
# we use the first 500 data as in-sample data in our selected series to train our parameters
# and use the last 500 data as out-sample data to test the performance of our optimized parameters

Inseries<- c(1,3,5,7,10)
# In-sample test for the first 500 data
INparams.rsi<- list(lookback=50, series=Inseries,posSizes=rep(0.01,10),n=14,lev=25)
results <- backtest(insample, getOrders.RSI, INparams.rsi, sMult)
pfolioPnL <- plotResults(insample,results)

# out-of sample test for the last 500 data
INparams.macd<- list(lookback=50, series=Inseries,posSizes=rep(0.01,10),n=14,lev=25)
outsample<- lapply(dataList, function(x) x[501:1000,])
results <- backtest(outsample, getOrders.RSI, INparams.macd, sMult)
pfolioPnL <- plotResults(insample,results)

# From the result, we can see a nice improvement on both PNL and PD ratio for our RSI strategy by using optimized parameters.
# However, grid-search method might also lead to over-fitting problem.
