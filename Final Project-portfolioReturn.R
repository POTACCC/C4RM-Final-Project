PortfolioReturns <- function(port, history){
  tickers     <- port$ticker
  stockMatrix <- as.matrix(history[,tickers])
  
  portfolios  <- stockMatrix %*% port$shares
  rows        <- nrow(history)
  returns     <- portfolios[2:rows] / portfolios[1:(rows-1)] - 1
  return(returns)
}

PortfolioWorstDay <- function(port, history){
  dates <- history$Date
  tickers     <- port$ticker
  stockMatrix <- as.matrix(history[,tickers])
  portfolios  <- stockMatrix %*% port$shares
  rows        <- nrow(history)
  returns     <- portfolios[2:rows] / portfolios[1:(rows-1)] - 1
  minreturn = returns[1]
  mindate = dates[2]
  for(i in 2:length(returns)){
    dailyreturn = returns[i]
    if(dailyreturn < minreturn){
      minreturn = dailyreturn
      mindate = dates[i+1]
    }
  }
  return(mindate)
}  
#read data from github and set up port
stockdata = read.csv("https://raw.githubusercontent.com/POTACCC/C4RM-Final-Project/main/russia_stock.csv")
energyport = data.frame(ticker = c("GAZP.USD", "LKOH.USD", "ROSN.USD"), shares = c(100,100,100))
financialport = data.frame(ticker = c("AVAN.USD", "USBN.USD", "RDRB.USD"), shares = c(100,100,100))
indexport = data.frame(ticker = c("IMOEX.ME"), shares = c(300))

date = stockdata$Date

energyReturn = PortfolioReturns(energyport, stockdata)
financialReturn = PortfolioReturns(financialport, stockdata)
indexReturn = PortfolioReturns(indexport, stockdata)
energyWorstDate = as.character(PortfolioWorstDay(energyport, stockdata))
financialdateWorstDate = as.character(PortfolioWorstDay(financialport, stockdata))
indexWorstDate = as.character(PortfolioWorstDay(indexport, stockdata))

#result
print(paste0("The worst return date for energy sector is ", energyWorstDate))
print(paste0("The worst return date for financial sector is ", financialdateWorstDate))
print(paste0("The worst return date for Russia Index is ", indexWorstDate))

plot(energyReturn, type = "l", col = "blue", lwd = 2, ylab = "Simple Return",
     main = "Daily Returns on energy")
plot(financialReturn, type = "l", col = "blue", lwd = 2, ylab = "Simple Return",
     main = "Daily Returns on financial")
plot(indexReturn, type = "l", col = "blue", lwd = 2, ylab = "Simple Return",
     main = "Daily Returns on Russia Index")



