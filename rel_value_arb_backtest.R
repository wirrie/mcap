#Raymond Wong
library(YieldCurve)
library(Quandl)

fedYieldCurve <- Quandl('FED/SVENPY', type = "xts")

fedYieldCurveModern <- fedYieldCurve["2009/2017"]
fedYieldCurveLostDecade <- fedYieldCurve["2000/2008"]
fedYieldCurve90sBull <- fedYieldCurve["1990/1999"]
fedYieldCurve80s <- fedYieldCurve["1980/1989"]#note, the maturities only go up to 15 years here for early 80s, so we only test 1 to 15 year maturities

#Graph the observed Fed Yield curve at 1981-12-31 and the fitted yield curve
#using the Nelson Siegel model
maturities <- c(1:30)
plot(maturities, fedYieldCurveModern[1,], type = "o",
     xlab = "Maturities Structure in Years",
     ylab = "Interest Rate Values")
title(paste("Fed Yield Curve Observed at ", index(fedYieldCurveModern[1,])))
NSParams <- Nelson.Siegel(fedYieldCurveModern[1,], maturities)
NSRates <- NSrates(NSParams, maturities) 
lines(maturities, NSRates, type = "o", col = 2)
spreads <- fedYieldCurveModern[1,]-NSRates

#This function takes rates and maturities and plots
#the yield curve and the fitted Nelson Siegel curve
plot_curves <- function(rates, maturities){
  plot(maturities, rates, type = "o",
       xlab = "Maturities Structure in Years",
       ylab = "Interest Rate Values")
  title(paste("Fed Yield Curve Observed at ", index(rates)))
  NSParams <- Nelson.Siegel(rates, maturities)
  NSRates <- NSrates(NSParams, maturities) 
  lines(maturities, NSRates, type = "o", col = 2)
}

plot_curves(fedYieldCurveModern[2,],maturities)
plot_curves(fedYieldCurveModern[1,],maturities)

#Build the xts containing spread data
spread_table <- list()
spread_table_lost_decade <- list()
spread_table_90s_bull <- list()
spread_table_80s <- list()


#This block of code takes a long time to run (approx 10 min) consider loading big_table
#into a csv if you dont want to do this every single time
#Pro-tip, since you will have to update this every day, instead of compiling big_table
#every day into a csv, just add the new yield curve to your old file
for(row in (1:nrow(fedYieldCurveModern))){
  rates = fedYieldCurveModern[row,]
  NSParams = Nelson.Siegel(rates, maturities)
  NSRates = NSrates(NSParams, maturities)
  spread = rates - NSRates
  spread_table[[row]] = spread
}

for(row in (1:nrow(fedYieldCurveLostDecade))){
  rates = fedYieldCurveLostDecade[row,]
  NSParams = Nelson.Siegel(rates, maturities)
  NSRates = NSrates(NSParams, maturities)
  spread = rates - NSRates
  spread_table_lost_decade[[row]] = spread
}

for(row in (1:nrow(fedYieldCurve90sBull))){
  rates = fedYieldCurve90sBull[row,]
  NSParams = Nelson.Siegel(rates, maturities)
  NSRates = NSrates(NSParams, maturities)
  spread = rates - NSRates
  spread_table_90s_bull[[row]] = spread
}

maturities_80s <- c(1:15)
for(row in (1:nrow(fedYieldCurve80s))){
  rates = fedYieldCurve80s[row,c(1:15)]
  NSParams = Nelson.Siegel(rates, maturities_80s)
  NSRates = NSrates(NSParams, maturities_80s)
  spread = rates - NSRates
  spread_table_80s[[row]] = spread
}

#The xts containing spread data, spread is distance from actual yield
#to the yield modeled by Nelson Siegel
big_table <- do.call(rbind,spread_table)
big_table_lost_decade <- do.call(rbind, spread_table_lost_decade)
big_table_90s_bull <- do.call(rbind, spread_table_90s_bull)
big_table_80s <- do.call(rbind, spread_table_80s)

#This function takes 
#-two maturities mat1 and mat2(are two periods apart)
#-df_spreads is a xts containing spread data
#-df_yields is a dataframe containing yield data
#-mat1spread (>0) is the minimum desired magnitude of difference from the modeled yield curve
#-mat2spread (>0) is the minimum desired magnitude of difference from the modeled yield curve
#-midspread (>0) is the maximum desired magnitude of difference from the yield curve of the maturity in the middle
#-mat1spread and mat2spread should have different signs
#-midspread should be small
#and returns a xts where each observation represents a yield curve
#where a relative value arb exists
findArbsRelVal <- function(mat1, mat2, df_spreads, df_yields, mat1spread, mat2spread, midspread){
  arb_list <- list()
  for(row in (1:nrow(df_yields))){
    mat1_observed_spread = df_spreads[row, mat1]
    mat2_observed_spread = df_spreads[row, mat2]
    midspread_observed = df_spreads[row, mat1+1]
    if(sign(mat1_observed_spread) != sign(mat2_observed_spread)){
      if(abs(mat1_observed_spread) > mat1spread & abs(mat2_observed_spread) > mat2spread){
        if(abs(midspread_observed) < midspread){
          arb_list[[row]] = df_yields[row,]
          #print(paste(mat1_observed_spread, mat2_observed_spread, midspread_observed, paste = " "))
        }
      }
    }
  }
  arb_table <- do.call(rbind, arb_list)
  return(arb_table)
}

arbs09_17 = findArbsRelVal(1, 3, big_table, fedYieldCurveModern, 0.1, 0.1, 0.02)
arbs2_09_17 = findArbsRelVal(1, 3, big_table, fedYieldCurveModern, 0.05, 0.05, 0.02)
arbs3_09_17 = findArbsRelVal(1, 3, big_table, fedYieldCurveModern, 0.04, 0.04, 0.02)
arbs4_09_17 = findArbsRelVal(1, 3, big_table, fedYieldCurveModern, 0.03, 0.03, 0.01)
arbs5_09_17 = findArbsRelVal(1, 3, big_table, fedYieldCurveModern, 0.02, 0.02, 0.005)
arbs6_09_17 = findArbsRelVal(1, 3, big_table, fedYieldCurveModern, 0.01, 0.01, 0.005)
arbs7_09_17 = findArbsRelVal(1, 3, big_table, fedYieldCurveModern, 0.01, 0.01, 0.02)

#This function takes 
#-df_arbs, xts of yield curves where arbitrage opportunities exist
#-df_yields, xts of yield curves over the time period of which we are testing
#-timeFrame, our holding time of the trade
#-mat1 is the first maturity
#-mat2 is the second maturity
#returns a xts with 
#row x column n being change of rates of trade x of maturity 1 at time n
#row x column 2n being change of rates of trade x of maturity 2 at time n
trackArbOpps <- function(df_arbs, df_yields, timeFrame, mat1, mat2){
  index = index(df_arbs)#the index of our returned xts needs to match the index tracking
  #our arb opportunities; our trades are tracked by the date which we execute them
  coreMatrix = matrix(ncol = timeFrame * 2, nrow = nrow(df_arbs))
  for(row in (1:nrow(df_arbs))){#go through each yield curve with arb opportunities
    orig_curve_date = index(df_arbs[row])
    sliced_df_yields = df_yields[paste(orig_curve_date,"/")]#create a smaller xts starting with the yield curve where the trade is executed
    i = 2
    while(i <= timeFrame+1){#put the rates over the specified timeframe into our xts
      if(i-1 > nrow(sliced_df_yields)){#if timeFrame goes further than we have data for
        #stop adding to the dataframe, just have NAs in place 
        break
      }
      coreMatrix[row, 2*(i-1)-1] = sliced_df_yields[i-1,mat1]
      coreMatrix[row, 2*(i-1)] = sliced_df_yields[i-1, mat2]
      i = i + 1
    }
  }
  final_xts = xts(x = coreMatrix, order.by = index)
  return(final_xts)
}

arbs09_17_opps = trackArbOpps(arbs09_17, fedYieldCurve, 5, 1, 3)
arbs2_09_17_opps = trackArbOpps(arbs2_09_17, fedYieldCurveModern, 5, 1, 3)
arbs3_09_17_opps = trackArbOpps(arbs3_09_17, fedYieldCurveModern, 5, 1, 3)
arbs4_09_17_opps = trackArbOpps(arbs4_09_17, fedYieldCurveModern, 5, 1, 3)
arbs5_09_17_opps = trackArbOpps(arbs5_09_17, fedYieldCurveModern, 5, 1, 3)
arbs6_09_17_opps = trackArbOpps(arbs6_09_17, fedYieldCurveModern, 5, 1, 3)
arbs7_09_17_opps = trackArbOpps(arbs7_09_17, fedYieldCurveModern, 5, 1, 3)

#This function takes
#- an xts tracking the yield rates over time of a trade
#and returns
#- an xts containing the change in their spread over time
#where index is date which represents the original date that we entered the trade
arbSpreadCalc <- function(arb_opps){
  index = index(arb_opps)
  days = ncol(coredata(arb_opps))/2
  rows = nrow(coredata(arb_opps))
  coreMatrix = matrix(ncol = days, nrow = rows)
  for(row in (1:nrow(arb_opps))){
    for(i in (1:days)){
      rate1 = arb_opps[row, 2*i-1]
      rate2 = arb_opps[row, 2*i]
      #print(paste(index(arb_opps[i,], rate1, rate2, sep = " ")))
      spread = abs(rate1-rate2)
      coreMatrix[row, i] = spread
    }
  }
  final_xts = xts(x = coreMatrix, order.by = index)
  return(final_xts)
}

arbs09_17_opps_spreads = arbSpreadCalc(arbs09_17_opps)
arbs2_09_17_opps_spreads = arbSpreadCalc(arbs2_09_17_opps)
arbs3_09_17_opps_spreads = arbSpreadCalc(arbs3_09_17_opps)
arbs4_09_17_opps_spreads = arbSpreadCalc(arbs4_09_17_opps)
arbs5_09_17_opps_spreads = arbSpreadCalc(arbs5_09_17_opps)
arbs6_09_17_opps_spreads = arbSpreadCalc(arbs6_09_17_opps)
arbs7_09_17_opps_spreads = arbSpreadCalc(arbs7_09_17_opps)

#This function
#-takes a xts containing the spread value of each day we are in the trade
#and returns
#-a dataframe containing average trade performance, win rate, average win,
#average loss, standard deviation of trade returns, number of trades taken
tradePerformance <- function(arb_spreads){
  df <- data.frame(matrix(ncol = 7, nrow = 1))
  #print(ncol(df))
  headers <- c("Avg Trade Perf.", "Win %", "Avg Win", "Loss %", "Avg Loss", "Std. Dev", "Num. of Trades")
  colnames(df) <- headers
  #trade_perf is a one column xts representing net change in basis points of your trade
  #which is entry spread value - exit spread value
  trade_perf <- arb_spreads[,1]-arb_spreads[,ncol(arb_spreads)]
  avg_trade <- mean(trade_perf, na.rm = TRUE)
  win_percent <- mean(trade_perf > 0, na.rm = TRUE)
  winning_trades <- trade_perf[,1] > 0
  win_trades <- trade_perf[winning_trades]
  avg_win <- mean(win_trades, na.rm = TRUE)
  loss_percent = 1 - win_percent
  losing_trades <- trade_perf[,1] <= 0
  lose_trades <- trade_perf[losing_trades]
  avg_loss <- mean(lose_trades, na.rm = TRUE)
  std_dev <- sd(trade_perf, na.rm = TRUE)
  num_trades <- nrow(trade_perf)
  stats <- c(avg_trade, win_percent, avg_win, loss_percent, avg_loss, std_dev, num_trades)
  df[1,] <- stats
  return(df)
}

print(tradePerformance(arbs09_17_opps_spreads))
print(tradePerformance(arbs2_09_17_opps_spreads))
print(tradePerformance(arbs3_09_17_opps_spreads))
print(tradePerformance(arbs4_09_17_opps_spreads))
print(tradePerformance(arbs5_09_17_opps_spreads))
print(tradePerformance(arbs6_09_17_opps_spreads))
print(tradePerformance(arbs7_09_17_opps_spreads))

results_list = list()

results_list[[1]] = tradePerformance(arbs09_17_opps_spreads)
results_list[[2]] = tradePerformance(arbs2_09_17_opps_spreads)
results_list[[3]] = tradePerformance(arbs3_09_17_opps_spreads)
results_list[[4]] = tradePerformance(arbs4_09_17_opps_spreads)
results_list[[5]] = tradePerformance(arbs5_09_17_opps_spreads)
results_list[[6]] = tradePerformance(arbs6_09_17_opps_spreads)
results_list[[7]] = tradePerformance(arbs7_09_17_opps_spreads)

#This function takes a 
#-list of backtest results
#-filename as a string
#and exports a csv containing all the results
exportResults <- function(listOfResults, fileName){
  results = list()
  for(i in (1:length(listOfResults))){
    curr_list = listOfResults[[i]]
    print(class(curr_list))
    results[[i]] = curr_list[1,]
  }
  final_results = do.call(rbind, results)
  colnames(final_results) = names(listOfResults[[1]])
  write.csv(final_results, file = fileName)
}

exportResults(results_list, "firstBacktest.csv")

#loop that we can use to automate the testing of many different relative value
#yield curve arbitrages at once
#Note to self, should make a rel val arb opp detector function

arb_opps_spreads = list()
plot_curves(fedYieldCurveModern["2017-12-22",], maturities)

#This function takes
#-df_yields, an xts of yield curves
#-df_spreads, an xts of spreads
#-mat1spread (>0) is the minimum desired magnitude of difference from the modeled yield curve
#-mat2spread (>0) is the minimum desired magnitude of difference from the modeled yield curve
#-midspread (>0) is the maximum desired magnitude of difference from the yield curve of the maturity in the middle
#-mat1spread and mat2spread should have different signs
#-midspread should be small
#and returns
#-a list of xts 
#-each xts contains yield curves where arb opportunities exist in the mat1 and mat 2 values
#1-3,2-4,3-5....28-30
arbDetector <- function(df_yields, df_spreads,  mat1spread, mat2spread, midspread){
  year_struc <- list()#will be a list of length 28, entry 1 is 1-3 trade, entry 2 is 2-4 trade, etc.
  for(i in (1:28)){
    print(i)
    struc = findArbsRelVal(i, i+2, df_spreads, df_yields, mat1spread, mat2spread, midspread)
    #print(class(struc))
    if(is.null(struc)){#if no trades were available for that specific structure
      struc = 0
    }
    year_struc[[i]] = struc
    names(year_struc)[i] = paste(i, i + 2, sep = "-")
    print(class(year_struc[[i]]))
  }
  return(year_struc)
}

rel_val_arbs_all_strucs <- arbDetector(fedYieldCurveModern, big_table, 0.03, 0.03, 0.01)

#workflow
#get the xts where each observation is a yield curve where a rel val arb opp exists
#track the curves
#calculate trade performance

#Second backtest 2018-01-17
#Independent variable is midspread
midspread = 0.005#we start with half of a basis point as our midspread (distance from Nelson Siegel)
#done on 1 and 3 year bonds
arbs_mid_1 <- findArbsRelVal(3,5,big_table, fedYieldCurveModern, 0.07, 0.07, midspread)
arbs_mid_2 <- findArbsRelVal(3,5,big_table, fedYieldCurveModern, 0.07, 0.07, 2*midspread)
arbs_mid_3 <- findArbsRelVal(3,5,big_table, fedYieldCurveModern, 0.07, 0.07, 3*midspread)
arbs_mid_4 <- findArbsRelVal(3,5,big_table, fedYieldCurveModern, 0.07, 0.07, 4*midspread)
arbs_mid_5 <- findArbsRelVal(3,5,big_table, fedYieldCurveModern, 0.07, 0.07, 5*midspread)
arbs_mid_6 <- findArbsRelVal(3,5,big_table, fedYieldCurveModern, 0.07, 0.07, 6*midspread)
arbs_mid_7 <- findArbsRelVal(3,5,big_table, fedYieldCurveModern, 0.07, 0.07, 7*midspread)
arbs_mid_8 <- findArbsRelVal(3,5,big_table, fedYieldCurveModern, 0.07, 0.07, 8*midspread)

arbs_mid_1_opps <- trackArbOpps(arbs_mid_1, fedYieldCurveModern, 5, 2, 4)
arbs_mid_2_opps <- trackArbOpps(arbs_mid_2, fedYieldCurveModern, 5, 2, 4)
arbs_mid_3_opps <- trackArbOpps(arbs_mid_3, fedYieldCurveModern, 5, 2, 4)
arbs_mid_4_opps <- trackArbOpps(arbs_mid_4, fedYieldCurveModern, 5, 2, 4)
arbs_mid_5_opps <- trackArbOpps(arbs_mid_5, fedYieldCurveModern, 5, 2, 4)
arbs_mid_6_opps <- trackArbOpps(arbs_mid_6, fedYieldCurveModern, 5, 2, 4)
arbs_mid_7_opps <- trackArbOpps(arbs_mid_7, fedYieldCurveModern, 5, 2, 4)
arbs_mid_8_opps <- trackArbOpps(arbs_mid_8, fedYieldCurveModern, 5, 2, 4)

arbs_mid_1_opps_spreads <- arbSpreadCalc(arbs_mid_1_opps)
arbs_mid_2_opps_spreads <- arbSpreadCalc(arbs_mid_2_opps)
arbs_mid_3_opps_spreads <- arbSpreadCalc(arbs_mid_3_opps)
arbs_mid_4_opps_spreads <- arbSpreadCalc(arbs_mid_4_opps)
arbs_mid_5_opps_spreads <- arbSpreadCalc(arbs_mid_5_opps)
arbs_mid_6_opps_spreads <- arbSpreadCalc(arbs_mid_6_opps)
arbs_mid_7_opps_spreads <- arbSpreadCalc(arbs_mid_7_opps)
arbs_mid_8_opps_spreads <- arbSpreadCalc(arbs_mid_8_opps)

results_list_mid = list()

results_list_mid[[1]] = tradePerformance(arbs_mid_1_opps_spreads)
results_list_mid[[2]] = tradePerformance(arbs_mid_2_opps_spreads)
results_list_mid[[3]] = tradePerformance(arbs_mid_3_opps_spreads)
results_list_mid[[4]] = tradePerformance(arbs_mid_4_opps_spreads)
results_list_mid[[5]] = tradePerformance(arbs_mid_5_opps_spreads)
results_list_mid[[6]] = tradePerformance(arbs_mid_6_opps_spreads)
results_list_mid[[7]] = tradePerformance(arbs_mid_7_opps_spreads)
results_list_mid[[8]] = tradePerformance(arbs_mid_8_opps_spreads)

exportResults(results_list_mid, "secondBacktest_mid_optimize.csv")

# for(i in (1:(length(rel_val_arbs_all_strucs)))){
#   if(nrow(rel_val_arbs_all_strucs[i]) != 0)){#if there actual trades to test
#     #arb_opps <- trackArbOpps(rel_val_arbs_all_strucs[i], fedYieldCurveModern, 5, )
#     fileName_struc <- names(rel_val_arbs_all_strucs[i])
#     #print(strsplit(fileName_struc, "-"))
#     fileName_ending <- "backtest.csv"
#     fileName = paste(fileName_struc, fileName_ending)
#   }
# }

#This function takes 
#-df_spreads, an xts containing spread data
#-timeFrame, our holding time of the trade, usually in days
#and returns an xts with 
#row x column n being change of rates of trade x of maturity 1 at time n
#row x column 2n being change of rates of trade x of maturity 2 at time n
#How this function defines a trade (relative value)
#1. Find the actual yield curve point who is closest to the NS model (selectivity based on the middle point of the relative val trade)
#2. Find the actual yield curve points who are the farthest from the NS model
#3. Track the performance of the trade for the given holding period (timeFrame)
trackMaxArbOpps <- function(df_spreads, timeFrame){
  index = index(df_spreads)#the index of our returned xts needs to match the index tracking
  #our arb opportunities; our trades are tracked by the date which we execute them
  coreMatrix = matrix(ncol = timeFrame * 2, nrow = nrow(df_spreads))
  for(row in (1:nrow(df_spreads))){
    mid_term <- which.min(apply(abs(df_spreads[row,]), MARGIN = 2, min))#the middle yield of our relative value trade
    spreads_before <- df_spreads[row,(1:mid_term)]#xts containing all spreads in tenors before the middle tenor
    spreads_after <- df_spreads[row,mid_term:ncol(big_table)]#xts containing all spreads in tenors after the middle tenor
    before_term <- which.min(apply(abs(spreads_before), MARGIN = 2, min))
    after_term <- which.min(apply(abs(spreads_after), MARGIN = 2, min))
    orig_curve_date = index(df_spreads[row])
    sliced_df_spreads = df_spreads[paste(orig_curve_date,"/")]#create a smaller xts starting with the yield curve where the trade is executed
    i = 2
    while(i <= timeFrame+1){#put the rates over the specified timeframe into our xts
      if(i-1 > nrow(sliced_df_spreads)){#if timeFrame goes further than we have data for
        #stop adding to the dataframe, just have NAs in place 
        break
      }
      coreMatrix[row, 2*(i-1)-1] = sliced_df_spreads[i-1,before_term]
      coreMatrix[row, 2*(i-1)] = sliced_df_spreads[i-1, after_term]
      i = i + 1
    }
  }
  final_xts = xts(x = coreMatrix, order.by = index)
  return(final_xts)
}

#Third backtest, note to self, run this backtest on 2000-2008 data
#Use the trackArbOpps function instead to find arbs
#The methodology of this test is different
#instead of specifying maturities and minimum spreads we want to make
#we just trade the best possible relative value arb that exists on the curve
#as in we get the smallest possible mid spread first (optimizing for midspread)
#then we get the biggest spreads on each side of the mid tenor regardless of their tenor

maxArbSpreads_5 <- trackMaxArbOpps(big_table, 5)
maxArbCalc_5 <- arbSpreadCalc(maxArbSpreads_5)
maxArbResults_5 <- tradePerformance(maxArbCalc_5)

maxArbSpreads_10 <- trackMaxArbOpps(big_table, 10)
maxArbCalc_10 <- arbSpreadCalc(maxArbSpreads_10)
maxArbResults_10 <- tradePerformance(maxArbCalc_10)

maxArbSpreads_15 <- trackMaxArbOpps(big_table, 15)
maxArbCalc_15 <- arbSpreadCalc(maxArbSpreads_15)
maxArbResults_15 <- tradePerformance(maxArbCalc_15)

maxArbSpreads_list <- vector("list", 30)

for(i in (31:60)){#i is the holding period of a trade
  maxArbSpreads <- trackMaxArbOpps(big_table, i)
  maxArbCalc <- arbSpreadCalc(maxArbSpreads)
  maxArbResults <- tradePerformance(maxArbCalc)
  maxArbSpreads_list[[i-30]] <- maxArbResults
}

exportResults(maxArbSpreads_list, "third_backtest_pt2.csv")

#Fourth backtest
#Same as third backtest except we are now testing on 2000-2008

maxArbSpreads_lost_decade_list <- vector("list", 60)
for(i in (1:60)){#i is the holding period of a trade
  maxArbSpreads <- trackMaxArbOpps(big_table_lost_decade, i)
  maxArbCalc <- arbSpreadCalc(maxArbSpreads)
  maxArbResults <- tradePerformance(maxArbCalc)
  maxArbSpreads_lost_decade_list[[i]] <- maxArbResults
}

exportResults(maxArbSpreads_lost_decade_list, "fourth_backtest.csv")

#Fifth backtest
#Same as third backtest except we are now testing on 1990-1999
maxArbSpreads_90s_bull_list <- vector("list", 60)
for(i in (1:60)){#i is the holding period of a trade
  maxArbSpreads <- trackMaxArbOpps(big_table_90s_bull, i)
  maxArbCalc <- arbSpreadCalc(maxArbSpreads)
  maxArbResults <- tradePerformance(maxArbCalc)
  maxArbSpreads_90s_bull_list[[i]] <- maxArbResults
}

exportResults(maxArbSpreads_90s_bull_list, "fifth_backtest.csv")

#Sixth backtest
#Same as third backtest except we are now testing on 1980-1989
#This doesn't work yet, but might be pointless to do now
maxArbSpreads_80s <- vector("list", 60)
for(i in (1:60)){#i is the holding period of a trade
  maxArbSpreads <- trackMaxArbOpps(big_table_80s, i)
  maxArbCalc <- arbSpreadCalc(maxArbSpreads)
  maxArbResults <- tradePerformance(maxArbCalc)
  maxArbSpreads_80s[[i]] <- maxArbResults
}

exportResults(maxArbSpreads_80s, "fifth_backtest.csv")
