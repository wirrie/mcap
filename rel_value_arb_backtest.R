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

for(row in (1:nrow(fedYieldCurveModern))){
  rates = fedYieldCurveModern[row,]
  NSParams = Nelson.Siegel(rates, maturities)
  NSRates = NSrates(NSParams, maturities)
  spread = rates - NSRates
  spread_table[[row]] = spread
}

#The xts containing spread data
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
findArbsWeekly <- function(mat1, mat2, df_spreads, df_yields, mat1spread, mat2spread, midspread){
  arb_list <- list()
  for(row in (1:nrow(df_yields))){
    mat1_observed_spread = df_spreads[row, mat1]
    mat2_observed_spread = df_spreads[row, mat2]
    midspread_observed = df_spreads[row, mat1+1]
    if(sign(mat1_observed_spread) != sign(mat2_observed_spread)){
      if(abs(mat1_observed_spread) > mat1spread & abs(mat2_observed_spread) > mat2spread){
        if(abs(midspread_observed) < midspread){
          arb_list[[row]] = df_yields[row,]
          print(paste(mat1_observed_spread, mat2_observed_spread, midspread_observed, paste = " "))
        }
      }
    }
  }
  arb_table <- do.call(rbind, arb_list)
  return(arb_table)
}

arbs09_17 = findArbsWeekly(1, 3, big_table, fedYieldCurveModern, 0.1, 0.1, 0.02)
arbs2_09_17 = findArbsWeekly(1, 3, big_table, fedYieldCurveModern, 0.05, 0.05, 0.02)
arbs3_09_17 = findArbsWeekly(1, 3, big_table, fedYieldCurveModern, 0.04, 0.04, 0.02)
arbs4_09_17 = findArbsWeekly(1, 3, big_table, fedYieldCurveModern, 0.03, 0.03, 0.01)
arbs5_09_17 = findArbsWeekly(1, 3, big_table, fedYieldCurveModern, 0.02, 0.02, 0.005)
arbs6_09_17 = findArbsWeekly(1, 3, big_table, fedYieldCurveModern, 0.01, 0.01, 0.005)

#This function takes 
#-df_arbs, xts of yield curves where arbitrage opportunities exist
#-df_yields, xts of yield curves over the time period of which we are testing
#-timeFrame, our holding time of the trade
#returns a xts with 
#row x column n being change of rates of trade x of maturity 1 at time n
#row x column 2n being change of rates of trade x of maturity 2 at time n
#Note to self, make sure to take care of the edge case where
#timeFrame goes further than we have data for
trackArbOpps <- function(df_arbs, df_yields, timeFrame){
  index = index(df_arbs)#the index of our returned xts needs to match the index tracking
  #our arb opportunities; our trades are tracked by the date which we execute them
  coreMatrix = matrix(ncol = timeFrame * 2, nrow = nrow(df_arbs))
  for(row in (1:nrow(df_arbs))){
    orig_curve = df_yields[index(df_arbs[row])]
    for(i in (1:timeFrame)){
      next_curve = 
    }
  }
}

trackArbOpps(arbs09_17, fedYieldCurve, 5)


