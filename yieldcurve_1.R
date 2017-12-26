#Raymond Wong
#Some sample code exploring the YieldCurve package
#Using the Nelson Siegel model to graph fitted curves
#and importing free data from Quandl
#Note to make more than 50 calls to Quandl per day you need to make a free account
#and get an API key
library(YieldCurve)
library(Quandl)

#Import data from R's YieldCurve package
data("ECBYieldCurve")
#Import data from Quandl
fedYieldCurve2 <- Quandl('FED/SVENPY', type = "xts")

first(ECBYieldCurve, '3 day')
last(ECBYieldCurve, '3 day')

#Maturities of the ECB Bond Yield xts
maturity.ECB <- c(3/12,6/12,1:30)

#Graph the first 3 and last 3 yield curves from the ECB data
par(mfrow=c(2,3))
for(i in c(1,2,3,653,654,655)){
  plot(maturity.ECB, ECBYieldCurve[i,], type = "p",
       xlab = "Maturities Structure in Years",
       ylab = "Interest Rates Values")
  title(main = paste("European Central Bank yield curve observed at ",
                     index(ECBYieldCurve[i])))
  grid()
}

#Graph the ECB Yield curve from 2007-01-08
par(mfrow = c(1,1))
plot(maturity.ECB,ECBYieldCurve["2007-01-08",], type = "o",
     xlab = "Maturities Structure in Years",
     ylab = "Interest Rates Values"
)
title(main = paste("European Central Bank yield curve observed at 2007-01-08"))

#Import Federal Reserve data R's YieldCurve package
data("FedYieldCurve")

#Graph the first 3 and last 3 yield curves from the ECB data
par(mfrow = c(2,3))
first(FedYieldCurve,'3 month')
last(FedYieldCurve,'3 month')
mat<-c(3/12, 0.5, 1,2,3,5,7,10)
par(mfrow=c(2,3))
for( i in c(1,2,3,370,371,372) ){
  plot(mat, FedYieldCurve[i,], type="o", xlab="Maturities structure in years", ylab="Interest rates values")
  title(main=paste("Federal Reserve yield curve obeserved at",time(FedYieldCurve[i], sep=" ") ))
  grid()
}

#Graph the observed Fed Yield curve at 1981-12-31 and the fitted yield curve
#using the Nelson Siegel model
par(mfrow = c(1,1))
date = "1981-12-31"
rates_1 <- FedYieldCurve[date,]
plot(mat, rates_1, type = "o", xlab = "Maturities Structure in Years", ylab = "Interest Rates Values")
title(main = paste("Fed Yield Curve observed at ", date))
NSParams <- Nelson.Siegel(rates_1, mat)
y <- NSrates(NSParams, mat)
lines(mat, y, type = "o", col = 2)
legend("bottomright",legend = c("observed yield curve", "fitted yield curve"), col = c(1,2), lty = 1, bty = "n")

#Graph the observed Fed Yield curve at 1981-12-31 and the fitted yield curve
#using the Nelson Siegel model
par(mfrow = c(1,1))
date = "2012-11-30"
rates_1 <- FedYieldCurve[date,]
plot(mat, rates_1, type = "o", xlab = "Maturities Structure in Years", ylab = "Interest Rates Values")
title(main = paste("Fed Yield Curve observed at ", date))
NSParams <- Nelson.Siegel(rates_1, mat)
y <- NSrates(NSParams, mat)
lines(mat, y, type = "o", col = 2)
legend("bottomright",legend = c("observed yield curve", "fitted yield curve"), col = c(1,2), lty = 1, bty = "n")


par(mfrow = c(3,3))

rows_ecb <- nrow(ECBYieldCurve)
row_vec <- c((rows_ecb-8):rows_ecb)

#Generate the 9 months starting from Nov 2008
start_month = as.yearmon("2008-11")
num_months = 9
months <- vector(length = num_months)
months[1] <- start_month
for(i in 2:(num_months)){
  curr_month = as.yearmon(months[i-1]+0.1)
  months[i] = curr_month
}

#Get the first observation of the first month of data in our sample (Nov 2008)
date = format(as.yearmon(months[1]), "%Y-%m")
last_9_months_fed = ECBYieldCurve[date][1]

#Get the other first observations of the other months of data in our sample (Dec 2008-July 2009)
for(i in c(2:length(months))){
  date = format(as.yearmon(months[i]), "%Y-%m")
  required_month = ECBYieldCurve[date]
  last_9_months_fed = rbind(last_9_months_fed, required_month[1])
}

#Plot the last 9 months of YC curves while keeping the fitted curve
#as the fitted curve we found in the first month
rows_ecb_monthly <- nrow(last_9_months_fed)

for(i in c(1:rows_ecb_monthly)){
  plot(maturity.ECB, last_9_months_fed[i,], type = "o",
       xlab = "Maturities Structure in Years",
       ylab = "Interest Rates Values")
  title(main = paste("ECB Yield Curve Observed at ",
                     index(last_9_months_fed[i])))
  spot_rates = last_9_months_fed[i,]
  spot_rates_1 = last_9_months_fed[rows_ecb_monthly-8]
  NSParams = Nelson.Siegel(spot_rates_1, maturity.ECB)
  y1 = NSrates(NSParams, maturity.ECB)
  lines(maturity.ECB, y1, type = "o", col = 3)
  NSParams = Nelson.Siegel(spot_rates, maturity.ECB)
  y = NSrates(NSParams, maturity.ECB)
  lines(maturity.ECB, y, type = "o", col = 2)
  legend("bottomright",legend = c("observed yield curve", "fitted yield curve"), col = c(1,2), lty = 1, bty = "n")
  grid()
}

