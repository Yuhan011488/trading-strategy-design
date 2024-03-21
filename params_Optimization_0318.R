# multi-mcga algorithm to find the best parameter set


# clean up the whole environment
rm(list = ls())

# install.packages("mcga")
library(mco)

epoch <- 0

evalFunc <- function(x) {
  s=Sys.time()  #timer
  source('D:/wenhe/preloading.R'); 
  
  epoch <<- epoch + 1
  
  params.opt <- list(lookback=100,  # watch window, default 100, means no trade at the first 100 days  
                     series=1:10,  # contol which series we want to trade, default 1:10
                     posSizes=c(x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9],x[10]),  # control series viable to TMA
                     long_holdPeriod=x[11],  # control maximum long holding period 
                     short_holdPeriod=x[12],  # control maximum short holding period 
                     TMA_MA1=x[13], TMA_MA2=x[14], TMA_MA3=x[15],  # control short/Mid/Long term signals for TMA
                     LMV_EMA1=x[16], LMV_EMA2=x[17], LMV_EMA3=x[18], LMV_EMA4=x[19], LMV_EMA5=x[20],  # control short/Mid/Long term signals for TMA
                     BBMR_SMA=x[21],  # control exit SMA for BBMR
                     BBMR_up_sdParam=x[22], BBMR_dn_sdParam=x[23],  # control Upper/Lower bounds for BBMR
                     BBTF_up_sdParam=x[24], BBTF_dn_sdParam=x[25],  # control Upper/Lower bounds for BBTF
                     srategyExit_params=x[26],  # controls maximum acceptable failing times for combined strategy
                     VaR_p=x[27], VaR_ret=x[28],  # control extreme risk measurement for VaR
                     ES_p=x[29], ES_ret=x[30],  # control extreme risk measurement for ES
                     trailingStops_long_param=x[31],  # control trailing history bound when Long Hold
                     trailingStops_short_param=x[32],  # control trailing history bound when Short Hold
                     Stopgain_long_param=x[33],  # control short-term Drawdown level when Long Hold
                     Stopgain_short_param=x[34],  # control short-term Drawdown level when Short Hold
                     Martingale=x[35])  # control martingale multiplier
  
  test.opt <- backtest(dataList, getOrders, params.opt, sMult=0.2)
  pfolioPnL.opt <- plotResults(dataList, test.opt, plotType='ggplot2',
                              titleString=paste('Strategy Performance, Epoch:',epoch))
  
  final_perf <- perfCalc(test=test.opt, pfolioPnL=pfolioPnL.opt)
  
  result <- numeric(4)
  result[1] <- -final_perf$pnl
  result[2] <- -final_perf$PDratio
  result[3] <- -final_perf$activeness
  result[4] <- -final_perf$expected_profit
  
  cat('****************************************')
  cat('\n')
  cat('epoch:',epoch)
  cat('\n')
  e=Sys.time()
  cat('runtime: ', e-s)
  cat('\n\n')
  cat('params:')
  print(params.opt)
  cat('pnl:', -result[1])
  cat('\n')
  cat('PDRatio:', -result[2])
  cat('\n')
  cat('avtiveness:', -result[3])
  cat('\n')
  cat('expected_profit:', -result[4])
  cat('\n')
  cat('****************************************')
  cat('\n')

  return(result)
}

# constrainFunc <- function(x) {
#   x[11] >= 1
#   x[11] <= 3
#   x[12] <= 8
#   x[12] >= 1
# }

res <- nsga2(fn = evalFunc, idim = 35, odim = 4, cdim=0,             # constraints=constrainFunc
             popsize = 100, generations=3,
             lower.bounds=c(0,0,0,0,0,0,0,0,0,0,  # posSizes
                            1,1,2,20,50,2,10,30,60,80,  # params 11-20
                            2,1,1,1,1,4,0.8,-0.2,0.8,-0.2,  # params 21-30
                            0.01,0.01,-0.2,0.01, 1.1),  # params 31-34
             upper.bounds=c(4000,200,100000,2000,1000,18000,1000,1200,1200,240,
                            10,10,10,50,100,10,30,60,80,100,
                            50,5,5,5,5,12,0.999,-0.01,0.999,-0.01,
                            0.2,0.2,-0.01,0.2, 3))

# 
# Result <- cbind(res[['par']],res[['value']])
# Result <- cbind(Result, res[['pareto.optimal']])
# write.csv(Result, "D:/wenhe/result.xls")

# x <- unlist(Result[1,1:40])