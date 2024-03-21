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
dataList <- getData(directory="PART1")
# part1<- getData(directory="PART1")
# part2<- getData(directory="PART2")
# part3<- getData(directory="PART3")

inSample <- lapply(dataList, function(x) x[1:500])
outSample <- lapply(dataList, function(x) x[501:1000])

dataList <- inSample

# basics exploration ------------------------------------------------------

# OHLCV for 10 products ---------------------------------------------------
op <- NULL;hp <- NULL;lp <- NULL;cp <- NULL;vol <- NULL;
op_vwap_5 <- NULL;op_vwap_30 <- NULL;op_vwap_100 <- NULL;
op_ema_5 <- NULL;op_ema_30 <- NULL;op_ema_100 <- NULL

for(i in 1:10){
  # OHLCV
  op<- cbind(op, dataList[[i]][,1])
  hp<- cbind(hp, dataList[[i]][,2])
  lp<- cbind(lp, dataList[[i]][,3])
  cp<- cbind(cp, dataList[[i]][,4])
  vol<- cbind(vol, dataList[[i]][,5])
  # Open Price VWAP
  temp_op_vwap_5 <- VWAP(dataList[[i]][,1],dataList[[i]][,5] ,5)
  temp_op_vwap_30 <- VWAP(dataList[[i]][,1],dataList[[i]][,5] ,30)
  temp_op_vwap_100 <- VWAP(dataList[[i]][,1],dataList[[i]][,5] ,100)
  op_vwap_5<- cbind(op_vwap_5, temp_op_vwap_5)
  op_vwap_30<- cbind(op_vwap_30, temp_op_vwap_30)
  op_vwap_100<- cbind(op_vwap_100, temp_op_vwap_100)
  # Open Price EMA
  op_ema_5 <- cbind(op_ema_5, EMA(op[,1],5))
  op_ema_30 <- cbind(op_ema_30, EMA(op[,1],30))
  op_ema_100 <- cbind(op_ema_100, EMA(op[,1],100))
}

# rename close price for each product as from "a" to "j"
colnames(op)<- c("a","b","c","d","e","f","g","h","i","j")
colnames(hp)<- c("a","b","c","d","e","f","g","h","i","j")
colnames(lp)<- c("a","b","c","d","e","f","g","h","i","j")
colnames(cp)<- c("a","b","c","d","e","f","g","h","i","j")
colnames(vol)<- c("a","b","c","d","e","f","g","h","i","j")
colnames(op_vwap_5)<- c("a","b","c","d","e","f","g","h","i","j")
colnames(op_vwap_30)<- c("a","b","c","d","e","f","g","h","i","j")
colnames(op_vwap_100)<- c("a","b","c","d","e","f","g","h","i","j")


# Open Price rolling mean exploration for 10 products at dataList -------
op_ma_5 <- rollmean(op, 5, fill = list(NA, NULL, NA), align = "right")
op_ma_30 <- rollmean(op, 30, fill = list(NA, NULL, NA), align = "right")
op_ma_100 <- rollmean(op, 100, fill = list(NA, NULL, NA), align = "right")


# Open Price rolling median exploration for 10 products at dataList -------
op_rm_5 <- rollmedian(op, 5, fill = list(NA, NULL, NA), align = "right")
op_rm_30 <- rollmedian(op, 30, fill = list(NA, NULL, NA), align = "right")
op_rm_100 <- rollmedian(op, 100, fill = list(NA, NULL, NA), align = "right")


# Open Price rolling max exploration for 10 products at dataList ----------
op_max_5 <- rollmax(op, 5, fill = list(NA, NULL, NA), align = "right")
op_max_30 <- rollmax(op, 30, fill = list(NA, NULL, NA), align = "right")
op_max_100 <- rollmax(op, 100, fill = list(NA, NULL, NA), align = "right")


# Overnight gap and Change from open to open exploration for 10 pr --------

overnightGap1 <- NULL
overnightGap <- NULL
for(i in 1:10){ 
  overnightGap1 <- dataList[[i]][,1] - stats::lag(dataList[[i]][,4])
  overnightGap <- cbind(overnightGap, overnightGap1)
}

# Open to Open exploration ------------------------------------------------
openDiff <- diff(op)

# rename the column in dataList as ("Open","High","Low","Close","Volume")
dataList<- lapply(dataList, as.quantmod.OHLC, 
                  col.names=c("Open","High","Low","Close","Volume"))

# Graphical Exploration ---------------------------------------------------

# open price --------------------------------------------------------------
basicStats(op) #descriptive stats
corrplot(cor(op),method="number")  #correlations
cov(op)  #covariances

# open price series
par(mfcol=c(3,2))
plot(op[,1])
plot(op[,2])
plot(op[,3])
plot(op[,4])
plot(op[,5])
plot(op[,6])
par(mfcol=c(2,2))
plot(op[,7])
plot(op[,8])
plot(op[,9])
plot(op[,10])


# high price --------------------------------------------------------------
basicStats(hp)
corrplot(cor(hp),method="number")

# open price series
par(mfcol=c(3,2))
plot(hp[,1])
plot(hp[,2])
plot(hp[,3])
plot(hp[,4])
plot(hp[,5])
plot(hp[,6])
par(mfcol=c(2,2))
plot(hp[,7])
plot(hp[,8])
plot(hp[,9])
plot(hp[,10])


# low price ---------------------------------------------------------------
basicStats(lp)
corrplot(cor(lp),method="number")

# open price series
par(mfcol=c(3,2))
plot(lp[,1])
plot(lp[,2])
plot(lp[,3])
plot(lp[,4])
plot(lp[,5])
plot(lp[,6])
par(mfcol=c(2,2))
plot(lp[,7])
plot(lp[,8])
plot(lp[,9])
plot(lp[,10])


# Volume ------------------------------------------------------------------
basicStats(vol)
corrplot(cor(vol),method="number")
cov(vol) 

# volume series
par(mfcol=c(2,1))
plot(vol[,1])
plot(vol[,2])
par(mfcol=c(2,1))
plot(vol[,3])
plot(vol[,4])
par(mfcol=c(2,1))
plot(vol[,5])
plot(vol[,6])
par(mfcol=c(2,1))
plot(vol[,7])
plot(vol[,8])
par(mfcol=c(2,1))
plot(vol[,9])
plot(vol[,10])

# EMA ---------------------------------------------------------------------
basicStats(op_ema_5)
corrplot(cor(op_ema_5),method="number")
cov(op_ema_5) 

s1_op_EMA5 <- EMA(op[,1],n=5)
s1_op_EMA30 <- EMA(op[,1],n=30)
s1_op_EMA100 <- EMA(op[,1],n=100)

Op <- merge(s1_op_EMA5,s1_op_EMA30,s1_op_EMA100,op[,1])

Op <- na.omit(Op)
plot.zoo(Op,screens=c(1,1,1),col=c("red","yellow",'blue','black'),
         xlab='',ylab='')
legend(x='topleft', bty = "n", lty = c(1,1,1,1), 
       col = c("red","yellow",'blue','black'),
       legend=c("s1_op_EMA5","s1_op_EMA30","s1_op_EMA100", "Op"))

# Daily spread ------------------------------------------------------------
dataList <- getData(directory="PART1")

daily_spread <- function(x){
  upspread<- abs(x$High-x$Open)/x$Open
  downspread<- abs(x$Low - x$Open)/x$Open
  c<- cbind(upspread,downspread)
  s<- apply(c,1, min) # take the lower value between upspread and downspread in every row
}
Hspread<- sapply(dataList, daily_spread)
Hspread<-as.xts(Hspread)
basicStats(Hspread)
cor(Hspread)
cov(Hspread)

# daily spread series
par(mfcol=c(2,1))
plot(Hspread[,1])
plot(Hspread[,2])
par(mfcol=c(2,1))
plot(Hspread[,3])
plot(Hspread[,4])
par(mfcol=c(2,1))
plot(Hspread[,5])
plot(Hspread[,6])
par(mfcol=c(2,1))
plot(Hspread[,7])
plot(Hspread[,8])
par(mfcol=c(2,1))
plot(Hspread[,9])
plot(Hspread[,10])

# plot histogram of spread
par(mfcol=c(3,2))
hist(Hspread[,1],breaks=100)
hist(Hspread[,2],breaks=100)
hist(Hspread[,3],breaks=100)
hist(Hspread[,4],breaks=100)
hist(Hspread[,5],breaks=100)
hist(Hspread[,6],breaks=100)
par(mfcol=c(2,2))
hist(Hspread[,7],breaks=100)
hist(Hspread[,8],breaks=100)
hist(Hspread[,9],breaks=100)
hist(Hspread[,10],breaks=100)


# exploration 1 -----------------------------------------------------------
# Look at the number of times after op > sma5 
# how many days on average it takes to 
# get back to the sma5 level 
# (so that we can better optimize our mean-reversion strategy)

series <- 1
up <- 0
dn <- 0
start_time <- NULL
stop_time <- NULL
time_dif <- NULL
date <- NULL

for (i in 5:500){
  if ((coredata(op[i,series]) > coredata(s1_op_EMA5[i,series]) ) 
      & ((i %in% date) == FALSE)){
    date <- append(date, i)
    up = up +1
    start_time <- i
    for (j in i:500){
      date <- append(date, j)
      if (coredata(op[j,series]) <= coredata(s1_op_EMA5[j,series])){
        stop_time <- j
        break
      }
    }
    time_dif[up] <- stop_time - start_time
    stop_time <- 0
    start_time <- 0
  }
}

mean(time_dif[0:(up-1)])
sum(time_dif[0:(up-1)])


# exploration 2 -----------------------------------------------------------
# By exploring OHLC, we discovered that
# Series 234 10 has very strong positive correlation, 789 strong positive correlation, 
# 10 and 789 have very strong negative correlation, 10 has a strong leading effect. 
# 5 and 4 have very strong negative correlation, 
# 1 and 6 have relatively independent distribution and 
# showed no strong correlation.
# Thus, we want to explore the number of times at t+1, 
# Series 234 and 789 up and down respectively 
# when each time Series 10 up, or down. 

up_10=0;up_2 =0;up_3=0;up_4=0;up_7=0;up_8=0;up_9=0
dn_10=0;dn_2=0;dn_3=0;dn_4=0;dn_7=0;dn_8=0;dn_9=0

for (i in 2:499){
  if (coredata(op[i,10]) > coredata(op[i-1,10])){
    up_10 = up_10 +1
    
    if (coredata(op[i+1,9]) > coredata(op[i,9])){
      up_9 = up_9 +1}
    else if (coredata(op[i+1,9]) < coredata(op[i,9])){
      dn_9 = dn_9 +1}
  }
  # if (coredata(op[i,10]) < coredata(op[i-1,10])){
  #   dn_10 = dn_10 +1
  #   if (coredata(op[i+1,9]) > coredata(op[i,9])){
  #   up_9 = up_9 +1}
  #   else if (coredata(op[i+1,9]) < coredata(op[i,9])){
  #   dn_9 = dn_9 +1}
  # }
}


# exploration 3 -----------------------------------------------------------
# We can explore how the open price changed after the surge volume, 
# whether it went up or down, as compared to yesterday. 
# A surge of volume can be defined as more than 50% change in volume 
# than that of the average of the last 7 days (short term) volume.

up_vol=0;dn_vol=0;
up_up=0;up_dn=0;dn_up=0;dn_dn=0;
series = 1

for (i in 2:499){
  # up surge of vol
  if ((coredata(vol[i,series])- coredata(vol[i-1,series]))
      /coredata(vol[i-1,series]) >0.5){
    up_vol = up_vol +1
    if ((coredata(op[i+1,series])-coredata(op[i,series])) 
        /coredata(op[i,series]) > 0.05){
      up_up = up_up +1}
    else if ((coredata(op[i+1,series])-coredata(op[i,series])) 
             /coredata(op[i,series]) < (-0.05)){
      up_dn = up_dn +1}
  }
  # down surge of vol
  if ((coredata(vol[i,series])- coredata(vol[i-1,series]))
      /coredata(vol[i-1,series]) <(-0.5)){
    dn_vol = dn_vol +1
    if ((coredata(op[i+1,series])-coredata(op[i,series])) 
        /coredata(op[i,series]) > 0.05){
      dn_up = dn_up +1}
    else if ((coredata(op[i+1,series])-coredata(op[i,series])) 
             /coredata(op[i,series]) < (-0.05)){
      dn_dn = dn_dn +1}
  }
}


# overnight gap -----------------------------------------------------------
basicStats(overnightGap)
par(mfcol=c(3,2))
plot(overnightGap[,1], type="l")
plot(overnightGap[,2], type="l")
plot(overnightGap[,3], type="l")
plot(overnightGap[,4], type="l")
plot(overnightGap[,5], type="l")
plot(overnightGap[,6], type="l")
par(mfcol=c(2,2))
plot(overnightGap[,7], type="l")
plot(overnightGap[,8], type="l")
plot(overnightGap[,9], type="l")
plot(overnightGap[,10], type="l")


# Open on Open Simple differences -----------------------------------------
basicStats(openDiff)
par(mfcol=c(3,2))
plot(openDiff[,1], type="l")
plot(openDiff[,2], type="l")
plot(openDiff[,3], type="l")
plot(openDiff[,4], type="l")
plot(openDiff[,5], type="l")
plot(openDiff[,6], type="l")
par(mfcol=c(2,2))
plot(openDiff[,7], type="l")
plot(openDiff[,8], type="l")
plot(openDiff[,9], type="l")
plot(openDiff[,10], type="l")


# changes in log returns --------------------------------------------------
oc_logdiff<- sapply(dataList, function(x) 100*
                      (log(as.numeric(x$Close[-1000]))-log(as.numeric(x$Open[-1]))))
apply(oc_logdiff,2, var)
par(mfcol=c(3,2))
plot(oc_logdiff[,1], type="l")
plot(oc_logdiff[,2], type="l")
plot(oc_logdiff[,3], type="l")
plot(oc_logdiff[,4], type="l")
plot(oc_logdiff[,5], type="l")
plot(oc_logdiff[,6], type="l")
par(mfcol=c(2,2))
plot(oc_logdiff[,7], type="l")
plot(oc_logdiff[,8], type="l")
plot(oc_logdiff[,9], type="l")
plot(oc_logdiff[,10], type="l")


# Statistics testing ------------------------------------------------------

# The Dickey-Fuller test  -------------------------------------------------

# A time series is said to be ??stationary?? if it has no trend, 
# exhibits constant variance over time, and has a constant autocorrelation structure over time.
# One way to test whether a time series is stationary is to perform an augmented Dickey-Fuller test
# Null hypothesis: there is unit root. (non-stationary)
# alternative hypothesis: stationary

ht <- ur.df(Hspread[,1])
attributes(ht)$teststat
attributes(ht)$cval

# we cannot reject the null hypothesis if 5pct of tau1 is less negative than its critical value


# The Ljung-Box test ------------------------------------------------------

# The Ljung-Box test is a test of randomness, 
# or a statistical test for the presence of lagged correlation in a time series.
# 1. Pure randomness test, p-value less than 5%, the series is non-white noise
# 2. It is used to test whether a series of observations in a certain time period are random independent observations. 
# If the observations are not independent of each other, 
# one observation may be correlated with another observation after i time units, 
# forming a relationship called autocorrelation

for (i in 1:10){
  Box.test(Hspread[,i], lag=10, type='Ljung')
}


# Cointegration Test ------------------------------------------------------

# If the difference between two sets of time series data is smooth, 
# then we can make profits based on the smoothness of the difference: 
# when the spread between two stocks is too large, 
# we expect the spread to converge based on the smoothness, 
# so we buy the low-priced stock and sell the high-priced stock short, 
# and wait for the price to return to make the reverse operation to profit.
# Null hypothesis:
# There are n cointegrated vectors

cotest<- ca.jo(op, type = c("eigen"))
summary(cotest)
slotNames(cotest)


# Decomposition -----------------------------------------------------------

# Decomposition procedures are used in time series 
# to describe the trend and seasonal factors in a time series. 
# More extensive decompositions might also include long-run cycles, 
# holiday effects, day of week effects and so on. Here, 
# we??ll only consider trend and seasonal decompositions.

# Decomposition
a <- ts(op[,1],frequency =365)
tsdisplay(a)
dc <- decompose(a)
plot(dc)
# season <- dc$figure
# plot(season,type='b',xaxt='n',xlab="")


# ARIMA -------------------------------------------------------------------

# ARIMA models are used to predict the future 
# (assuming that the future will repeat the historical trend) 
# by finding the autocorrelation between historical data, 
# requiring the series to be smooth
# If this model fits well, it probably means that we can use this model to predict the future price.

fit <- auto.arima(a)
print(fit)
checkresiduals(fit)

forecast1 <- forecast(fit,h=200,level=c(95))
accuracy(fit)
forecast1
par(mfcol=c(2,1))
plot(forecast1)
plot(a)


# Trading Indicator -------------------------------------------------------

# Signals are used primarily to describe price movements or trend of a security, derivative, or currency over time. 

# plot MACD for 10 products
# chartSeries(dataList[[3]][1:150,], theme=chartTheme('white'), TA=c(addMACD()))
for (i in 1:10){
  chartSeries(dataList[[i]], theme=chartTheme('white'), TA=c(addMACD()))
  title(c('dataList product',i))
}

# plot RSI for 10 products
for (i in 1:10){
  chartSeries(dataList[[i]], theme=chartTheme('white'), TA=c(addRSI()))
  title(c('dataList product',i))
}

# plot BBands for 10 products
# bb1<- BBands(dataList[[1]][,2:4])
for (i in 1:10){
  chartSeries(dataList[[i]][,2:4], theme=chartTheme('white'), TA=c(addBBands(n=20,sd=2)))
  title(c('dataList product',i))
}

# plot OBV for 10 products
for (i in 1:10){
  chartSeries(dataList[[i]][,1:5], theme=chartTheme('white'), TA=c(addOBV()))
  title(c('dataList product',i))
}

# plot CMF for 10 products
for (i in 1:10){
  chartSeries(dataList[[i]][,1:5], theme=chartTheme('white'), TA=c(addCMF()))
  title(c('dataList product',i))
}

# plot ATR for 10 products
for (i in 1:10){
  chartSeries(dataList[[i]][,1:5], theme=chartTheme('white'), TA=c(addATR()))
  title(c('dataList product',i,'ATR'))
}

# plot Volumn for 10 products
for (i in 1:10){
  chartSeries(dataList[[i]][,1:5], theme=chartTheme('white'), TA=c(addVo()))
  title(c('dataList product',i))
}

# plot Momentum for 10 products
for (i in 1:10){
  chartSeries(dataList[[i]][,1:5], theme=chartTheme('white'), TA=c(addMomentum()))
  title(c('dataList product',i))
}

# plot SMA for 10 products 
# Candlestick charts are a visual aid for decision making in stock, foreign exchange, commodity, and option trading.
for (i in 1:10){
  candleChart(dataList[[i]][1:1000,],  theme = "white")
  addSMA(n = c(20, 50, 200))
  title(c('dataList product',i,'SMA'))
}
