from csv import reader
from collections import OrderedDict

bonds = OrderedDict()

counter = 0

currentDate = ""

bondsThatCanBeTraded = []

values = []
dates = []

bondsBought = OrderedDict()

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
        

def tradeableBonds():
    global bonds
    global currentDate
    global bondsThatCanBeTraded
    
    bondsThatCanBeTraded = []
    for bond in bonds:
        if (currentDate in (list(bonds[bond].keys()))):
            bondsThatCanBeTraded.append(bond)


def pricesLastNumDays(bond, number):            
    global currentDate
    prices = []
    
    currentIndex = int(list(bonds[bond].keys()).index(currentDate))
    lowestIndex = currentIndex - number
    if lowestIndex < 0:
        lowestIndex = 0
    while (currentIndex > lowestIndex):
        prices.append(float(bonds[bond][(list(bonds[bond].keys())[currentIndex])]))
        currentIndex = currentIndex - 1
    
    while (len(prices) < number):
        prices.append(-1)
    
    prices.reverse()
    
    return prices


def getAverage(prices):
    avg = 0
    for price in prices:
        avg = avg + price
    
    avg = float(avg)/len(prices)
    return avg

def movingAverageCrossover():
    global bondsThatCanBeTraded
    global currentDate
    global bondsBought
    
    firstMovingAverage = 50
    secondMovingAverage = 200
    
    for bond in bondsThatCanBeTraded:
        firstPrices = pricesLastNumDays(bond, firstMovingAverage)
        secondPrices = pricesLastNumDays(bond, secondMovingAverage)
        
        if (secondPrices[0] != -1):
            firstAvg = getAverage(firstPrices)
            secondAvg = getAverage(secondPrices)
        
            if (firstAvg > secondAvg):
                if (bond not in (list(bondsBought.keys()))):
                    bondsBought[bond] = (float(bonds[bond][currentDate]))
                
def historicalPeakAndTrough (prices):
    trough = prices[0]
    peak = prices[0]
    
    for price in prices:
        if (price > peak):
            peak = price
        
        if (price < trough):
            trough = price
            
    
    return peak, trough


def macd():
    global bondsThatCanBeTraded
    
    twentySixDayEMALength = 26
    twelveDayEMALength = 12
    
    for bond in bondsThatCanBeTraded:
        prices = pricesLastNumDays(bond, twentySixDayEMALength)
        
        simpleMovingAverage = getAverage(prices)
        
        previousExp = simpleMovingAverage
        
        smoothingFactor = 2/(1+twentySixDayEMALength)
        
        ema26 = (float(bonds[bond][currentDate])*smoothingFactor) + (previousExp*(1-smoothingFactor))
        
        prices = pricesLastNumDays(bond, twelveDayEMALength)
        simpleMovingAverage = getAverage(prices)
        
        previousExp = simpleMovingAverage
        
        smoothingFactor = 2/(1 + twelveDayEMALength)
        ema12 = (float(bonds[bond][currentDate])*smoothingFactor) + (previousExp*(1-smoothingFactor))
        
        difference  = ema12 - ema26
        
        if (difference > 0):
            if (bond not in (list(bondsBought.keys()))):
                    bondsBought[bond] = (float(bonds[bond][currentDate]))
        
               
def fibonacciSequence():
    global bondsThatCanBeTraded
    for bond in bondsThatCanBeTraded:
        prices = pricesLastNumDays(bond, 90)
        if (prices[0] != -1):
            peak, trough = historicalPeakAndTrough(prices)
            difference = peak - trough
            if (float(bonds[bond][currentDate]) < trough + difference*0.236):
                if (bond not in (list(bondsBought.keys()))):
                    bondsBought[bond] = (float(bonds[bond][currentDate]))
    return 0

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
    global currentDate
    global weeks
    global values
    global dates
    global bondsBought
    
    weeks = 0
    toContinue = nextDay()
    
    daysPast = 0
    
    start = False
    
    profit = 0
    loss = 0
    
    avgWin = 0
    avgLoss = 0
    
    while(toContinue != -1):
        if ("17-01" in currentDate):
            start = True
            
        if ("18-01" in currentDate):
            start = False
        
        if (daysPast%5 == 0 and start):
            print (currentDate)
            for bond in bondsBought:
                if (currentDate in list(bonds[bond].keys())):
                    if (float(bonds[bond][currentDate]) > bondsBought[bond]):
                        profit = profit + 1
                        avgWin = avgWin + (float(bonds[bond][currentDate]) - bondsBought[bond])/bondsBought[bond]
                    elif (float(bonds[bond][currentDate]) < bondsBought[bond]):
                        loss = loss + 1
                        avgLoss = avgLoss + (float(bonds[bond][currentDate]) - bondsBought[bond])/bondsBought[bond]
            
            bondsBought = OrderedDict()
            
            tradeableBonds()
            
            #movingAverageCrossover()
            #fibonacciSequence()
            macd()
    
        toContinue = nextDay()
        daysPast = daysPast + 1
    
    print (str(profit/(profit + loss) * 100)  + " % Win Rate")
    print ("Average Win: " + str(avgWin/profit * 100) + " %")
    print ("Average Loss: " + str(avgLoss/loss * 100) + " %")

run()     