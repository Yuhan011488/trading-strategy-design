# trading-strategy-design
该项目深入研究均值回归和趋势交易策略，分析其在预测股价走势和优化投资回报方面的有效性。研究还包括对风险管理技术和头寸大小策略的详细审查，以减少潜在损失，实现盈利最大化。

There are 7 R Files and 1 Excel file and 2 File Folders in our portfolio.

############################################################################################################
R Files:

1. part1_0207.R
contains the main code for assignment 1
including descriptive exploration, indicators exploration, strategy exploration and statistics testing

2. workingScript_0308.R
contains single BBands Trend-following strategy for assignment 2

3. params_Optimization_0318.R
contains Genetic Algorithm to optimize parameters for assignment 2

4. part2_wip_0308.r
contains preliminary implementations of 4 strategies based on 'workingScript_0308.R'

5. preloading.R
contains Environment Cleaning, Library Installing, Library Loading, Data Loading, 
Intilize Global Variables and functions needed for R files 'workingScript_0308.R', 'params_Optimization_0318.R', 'part2_wip_0308.r' and 'combined_ws_0318.R'
and Implementations of 4 Trading Strategies

6. combined_ws_0318.R
contains the main trading system design code

7. team4_v4.R
contains final submitted strategy for assignment 3

############################################################################################################
Excel file:

1. result.xls
contains the final 100 best results(parameter sets) after Genetic Algorithm optimization of parameters

############################################################################################################
File folders:

1. part1_perf_analysis
contains the 4 trading process files of final submitted strategy on part 1 data: 
flagStore_part1, logging everyday trading status for 10 series
marketOrderStore_part1, logging everyday market orders sent by our trading system to the Backtester framework for 10 series
moneySpendStore_part1, logging everyday total money spent of our trading system on 10 series
SOCStore_part1, logging everyday trading strategies in control for 10 series

2. part3_perf_analysis
contains the 4 trading process files of final submitted strategy on part 3 data:  
flagStore_part3, logging everyday trading status for 10 series
marketOrderStore_part3, logging everyday market orders sent by our trading system to the Backtester framework for 10 series
moneySpendStore_part3, logging everyday total money spent of our trading system on 10 series
SOCStore_part3, logging everyday trading strategies in control for 10 series

############################################################################################################
Before running the code, several things need to be noted:
(1) First you need to copy all R files in our portfolio under this directory 'D:/', and open them all up in RStudio
(2) Set the R working directory in backtester_v5.6 file directory
(3) Make sure you have data.R, backtester.R, processResults.R, utilities.R files in backtester_v5.6\framework\
(4) Set '\backtester_v5.6' as your current RStudio working directory
(5) Install all needed R library as noted in R file 'preloading.R' line 9-27 and their dependent packages before run the code
(6) Copy the PART1, PART2, PART3 data files to the directory 'backtester_v5.6\DATA\' and each data file should contain 10 different series csv files
(7) Follow the comments on each R file to run it








