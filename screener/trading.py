from csv import reader
from collections import OrderedDict
from math import floor
import matplotlib.pyplot as plt
import datetime
import numpy as np


bonds = OrderedDict()

counter = 0

currentDate = ""

stopLoss = 0.02

lookback = 20

momentumLongs = OrderedDict()

bondsThatCanBeTraded = []

cash = 10000
portfolioValue = cash

fee = 0.10

values = []
dates = []

with open('bondData.csv') as csvfile:
    spamreader = reader(csvfile, delimiter=',')
    for row in spamreader:
        count = 0
        bond = 0
        for cell in row:
            if (counter == 0 and count%3 == 0):
                bonds[cell] = OrderedDict()
            elif (count%3 == 0):
                bonds[(list)(bonds.keys())[bond]][cell] = row[count + 1] 
                bond = bond + 1
            count = count + 1
        counter = counter + 1
    
def momentum():
    global lookback
    global bondsThatCanBeTraded
    global currentDate
    
    for bond in bondsThatCanBeTraded:
        rsis = []
        bondStillTradeable = True
        
        for i in range(lookback):
            pIndex = list(bonds['CM 1.55 01/23/2018 Corp']).index(currentDate)
            if (pIndex - i*lookback > -1 and bondStillTradeable):
                newIndex = pIndex - i*lookback
                pDate = (list(bonds['CM 1.55 01/23/2018 Corp'].keys()))[newIndex]
                if (pDate in (list(bonds[bond].keys()))):
                    prices = pricesLastWeeks(pDate, lookback, bond)
                        
                    if prices[0] == -1:
                        bondStillTradeable = False
                    elif (bondStillTradeable):
                        rsis.append(rsi(prices))
                        if (rsis[len(rsis) - 1] == -1):
                            bondStillTradeable = False
                else:
                    bondStillTradeable = False
            else:
                bondStillTradeable =  False  
        
        if (bondStillTradeable):
            lowestRSI = rsis[0]
            highestRSI = rsis[0]
            for i in range(len(rsis)):
                if (rsis[i] < lowestRSI):
                    lowestRSI = rsis[i]
                elif (rsis[i] > highestRSI):
                    highestRSI = rsis[i]
                    
            
            stochRSI = (rsis[0] - lowestRSI)/(highestRSI - lowestRSI)

            if (stochRSI < 0.1):
                print (bond)
                if (bond not in (list(momentumLongs.keys()))):
                    buyMomentum(bond, float(bonds[bond][currentDate]))
            elif (stochRSI > 0.6):
                if (bond in (list(momentumLongs.keys()))):
                    sellMomentum(bond, float(bonds[bond][currentDate]))
                    
            if (bond in (list(momentumLongs.keys()))):
                buyPrice = float(momentumLongs[bond].split("-")[0])
                currentPrice = float(bonds[bond][currentDate])
                difference = (currentPrice - buyPrice)/buyPrice
                if (difference < -stopLoss):
                    sellMomentum(bond, currentPrice)
        
    return 0

def buyMomentum(bond, price):
    global momentumLongs
    global cash
    global currentDate
    
    if (cash > price):
        shares = floor(cash/price)
        momentumLongs[bond] = str(price) + "-" + str(shares)
        cash = cash - shares*price - fee*shares
        if ("17-12" in currentDate):
            print (bond)

def sellMomentum(bond, price):
    global momentumLongs
    global profitOrLoss
    global currentDate
    global cash
    
    shares = float(momentumLongs[bond].split("-")[1])
    
    cash = cash + shares*price - fee*shares
    del momentumLongs[bond]
       
def tradeableBonds():
    global bonds
    global currentDate
    global bondsThatCanBeTraded
    
    bondsThatCanBeTraded = []
    for bond in bonds:
        if (currentDate in (list(bonds[bond].keys()))):
            bondsThatCanBeTraded.append(bond)


def pricesLastWeeks(date, weeks, bond):
    global currentDate
    prices = []
    currentIndex = int(list(bonds[bond].keys()).index(date))
    lowestIndex = currentIndex - weeks
    if lowestIndex < 0:
        lowestIndex = 0
    while (currentIndex > lowestIndex):
        prices.append(float(bonds[bond][(list(bonds[bond].keys())[currentIndex])]))
        currentIndex = currentIndex - 1
    
    while (len(prices) < weeks):
        prices.append(-1)
    
    prices.reverse()
    
    return prices


def rsi(prices):
    loss = []
    gain = []
    for i in range(len(prices) - 1):
        difference = (prices[i + 1] - prices[i])/prices[i]
        if difference < 0:
            loss.append(difference)
        elif difference > 0:
            gain.append(difference)
    
    
    
    if ((len(gain)) == 0 or (len(loss)) == 0):
        return -1
    
    avgLoss = 0
            
    for i in range(len(loss)):
        avgLoss = avgLoss - loss[i]
    
    avgLoss = avgLoss/((float)(len(loss)))
    
    avgGain = 0
    for i in range (len(gain)):
        avgGain = avgGain + gain[i]
        
    avgGain = avgGain/((float)(len(gain)))
    
    rs = avgGain/avgLoss
    
    relativeStrengthIndex = 100 - (100/(1+rs))
    
    return relativeStrengthIndex

def nextDay():
    global weeks
    global currentDate
    currentDate =  (list(bonds['CM 1.55 01/23/2018 Corp'].keys())[weeks])
    weeks = weeks + 1
    if (currentDate == "17-12-26"):
        return -1
    else:
        return 1

def run():
    global profitOrLoss
    global currentDate
    global weeks
    global values
    global dates
    
    weeks = 0
    toContinue = nextDay()
    
    daysPast = 0
    
    start = False
    
    while(toContinue != -1):
        if ("17-01" in currentDate):
            start = True
            
        if ("18-01" in currentDate):
            start = False
        
        if (daysPast%1 == 0 and start):
            print (currentDate)
            tradeableBonds()
            momentum()
            pv = getPortfolioValue()
            
            if (pv == 0):
                dtDate = datetime.datetime.strptime("20"+currentDate,"%Y-%m-%d")
                dates.append(dtDate)
                values.append(portfolioValue)
                print (portfolioValue)
                
        
        
        toContinue = nextDay()
        daysPast = daysPast + 1
    
    
    
    dates = np.array(dates)
    values = np.array(values)
    
    print ((len)(dates))
    print ((len)(values))
    plt.plot(dates, values)
    plt.show()
    
def getPortfolioValue():
    global cash
    global momentumLongs
    global portfolioValue
    global bonds
    global currentDate
    
    portfolioValue = cash
    for bond in ((list)(momentumLongs.keys())):
        if (currentDate in bonds[bond]):
            currentPrice = float(bonds[bond][currentDate])
            portfolioValue = portfolioValue + currentPrice*float(momentumLongs[bond].split("-")[1])
        else:
            return -1
    
    return 0

run()