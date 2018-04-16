##################################
## Upload the Required Packages ##
##################################

# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("gmm")
# install.packages("stats")
# install.packages("graphics")

library(data.table)
library(ggplot2)
library(gmm)
library(stats)
library(graphics)
library(quantmod)
library(PerformanceAnalytics)

#############################################
## Stock Returns From Different Industries ##
#############################################

# APPLE             (APPL)
# AETNA             (AET)
# Bank of America   (BAC)
# CocaCola          (KO)
# DISNEY            (DIS)
# EXXON             (XOM)
# FORD              (FORD)
# GENERAL ELECTRIC  (GE)
# IBM               (IBM)
# INTEL             (INTC)
# TOYOTA            (TM)
# UNION PACIFIC     (UNP)
# WALMART           (WMT)

###############################
## Import Stock Returns Data ##
###############################

data <- read.csv("/Users/armandkapllani/Desktop/Stocks/data.csv", header = TRUE)

###############################
## Omit Missing Observations ##
###############################


symbols_fred <- c("TB1YR", "PCEPILFE", "CPIAUCSL")
symbols_stocks <- c("AAPL", "AET", "BAC", "KO", "DIS", "XOM", "GE", "IBM", "INTC", 
                    "TM", "UNP", "WMT")


# Obtain stock data from Yahoo Finance

stocks <- lapply(symbols_stocks, function(sym) {
  monthlyReturn(na.omit(getSymbols(sym, from = "1981-01-01", to = "2017-12-01", 
                                   auto.assign=FALSE, src = "yahoo")))
})

# FRED does not allow range specification 

getSymbols(symbols_fred, src = "FRED")

# Consumption (C)
C <- data.frame(date=index(PCEPILFE), coredata(PCEPILFE))
C <- data.table(C)
setnames(C, c("Date", "C"))
C <- C[Date > "1980-11-01"]

# Ctilde
Ctilde <- matrix(0, nrow = nrow(C)-1, ncol = 1)
for (t in 1:nrow(Ctilde)) {
  Ctilde[t] <- C[,2][t+1]/C[,2][t]
}
Ctilde <- data.table(Ctilde)

# CPI (CPI)
CPI <- data.frame(date=index(CPIAUCSL), coredata(CPIAUCSL))
CPI <- data.table(CPI)
setnames(CPI, c("Date", "CPI"))
CPI <- CPI[Date >= "1980-12-01"]
    
# Inflation (infl)
infl <- matrix(0, nrow(CPI)-1, ncol = 1)
for (t in 1:nrow(CPI)) {
  infl[t] <- (CPI[,2][t+1] - CPI[,2][t])/CPI[,2][t]
}
infl <- data.table(infl)
setnames(infl, "Inflation")
infl <- infl[Inflation != "NA"]
infl <- as.numeric(unlist(infl))
infl <- matrix(infl, ncol = 1)

# TB1YR (TB1YR)
TB1YR <- data.frame(date=index(TB1YR), coredata(TB1YR))
TB1YR <- data.table(TB1YR)
setnames(TB1YR, c("Date", "TB1YR"))
TB1YR <- TB1YR[Date > "1980-12-31"]

# Create a Dataset of Stocks         
stocks <- do.call(cbind, c(stocks))
stocks <- data.frame(date=index(stocks), coredata(stocks))
stocks <- data.table(stocks)
setnames(stocks, c("Date", symbols_stocks))
charts.PerformanceSummary(stocks)
chart.Histogram(stocks, main = "Density", breaks=40, methods = c("add.density", "add.normal"))

# Discount the Stock Returns by CPI
\[
 \text{real rate of return} = \frac{1 + \text{nomial rate}}{1 + inflation rate} - 1 
\]

dta <- data.table(Ctilde, stocks[, Date := NULL], infl)
stocks_disc <- data.frame(stocks)

for (j in 1:ncol(stocks_disc)){
  stocks_disc[,j] <- (1 + stocks_disc[,j])/(1 + infl) - 1
}






dta <- data.table(stocks[, Date := NULL], TB1YR[, Date := NULL])









