library (YieldCurve)
library (Quandl)
### plot ECB Yield Curve ###

Quandl.api_key("TpyokPCf4EshqWN1LcKs")
print("Loading data")
twoYearECB = Quandl("ECB/FM_M_U2_EUR_4F_BB_U2_2Y_YLD")
threeYearECB = Quandl("ECB/FM_M_U2_EUR_4F_BB_U2_3Y_YLD")
fiveYearECB = Quandl("ECB/FM_M_U2_EUR_4F_BB_U2_5Y_YLD")
sevenYearECB = Quandl("ECB/FM_M_U2_EUR_4F_BB_U2_7Y_YLD")
tenYearECB = Quandl("ECB/FM_M_U2_EUR_4F_BB_U2_10Y_YLD")
print("Loaded data")
ecb = c(twoYearECB[1,"Percent per annum"], threeYearECB[1,"Percent per annum"], fiveYearECB[1,"Percent per annum"], sevenYearECB[1,"Percent per annum"], tenYearECB[1,"Percent per annum"])
maturities = c(24,36,60,85,120)

ecbData = data.frame(maturities, ecb)

plot(ecbData, type = "o", ylim=c(-0.5,15))


frame <- Nelson.Siegel(ecb, maturities)
print (frame)
print ("Nelson-Siegel")
for (item in 2:nrow(twoYearECB)){
  if (!grepl("198", twoYearECB[item,"Date"]) && !grepl("2011", twoYearECB[item,"Date"])  && !grepl("2012", twoYearECB[item,"Date"]) && !grepl("1991", twoYearECB[item,"Date"]) && !grepl("1992", twoYearECB[item,"Date"])){
    ecb = c(twoYearECB[item,"Percent per annum"], threeYearECB[item,"Percent per annum"], fiveYearECB[item,"Percent per annum"], sevenYearECB[item,"Percent per annum"], tenYearECB[item,"Percent per annum"])
    # ecbData = data.frame(maturities, ecb)
    # points(ecbData, type = "o")
    # Sys.sleep(0.05)
    # print (twoYearECB[item,"Date"])
    frame <- rbind(frame, Nelson.Siegel(ecb, maturities))
    print(item)
  }
}
print ("Finished Nelson-Siegel")
print (frame)
write.table(frame, "coefficients.txt", sep=",")