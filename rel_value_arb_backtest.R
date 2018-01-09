#Raymond Wong
library(YieldCurve)
library(Quandl)

fedYieldCurve <- Quandl('FED/SVENPY', type = "xts")

fedYieldCurveModern <- fedYieldCurve["2009/2017"]

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

#Build the xts containing spread data
spread_table <- list()

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

#The xts containing spread data, spread is distance from actual yield
#to the yield modeled by Nelson Siegel
big_table <- do.call(rbind,spread_table)

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
#-mat1spread (>0) is the minimum desired magnitude of difference from the modeled yield curve
#-mat2spread (>0) is the minimum desired magnitude of difference from the modeled yield curve
#-midspread (>0) is the maximum desired magnitude of difference from the yield curve of the maturity in the middle
#-mat1spread and mat2spread should have different signs
#-midspread should be small
#and returns
#-a list of xts 
#-each xts contains yield curves where arb opportunities exist in the mat1 and mat 2 values
#1-3,2-4,3-5....28-30
arbDetector <- function(df_yields, mat1spread, mat2spread, midspread){
  
}
