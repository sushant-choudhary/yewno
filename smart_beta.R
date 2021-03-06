# SPECIFY INPUTS

start <- as.Date("2010-12-31") 
end <- as.Date("2017-12-31")

# The following 3 specify how many of the top contenders out 
# of 30 DJIA components should be selected for the smart beta strategy
n_momentum <- 15
n_lowvol <- 15
n_combination <- 15

# IMPORT LIBRARIES

library(quantmod)
library(purrr)
library(xml2)
library(rvest)

# DEFINE HELPER FUNCTIONS

isTopN<-function(vec, N){
  1+length(vec)-rank(vec) <= N
}

isBottomN<-function(vec, N){
  rank(vec) <= N
}

get_DJ_symbols<-function(){
  wiki<-"https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average"
  htmldocobj<-read_html(wiki)
  DJIA <- html_table(htmldocobj, fill=T)[[2]]
  companies <- DJIA$Company
  symbols <- DJIA$Symbol
  names(symbols)<-companies
  return(symbols)
}

getPriceSeries<-function(symbol) {
  getSymbols(symbol, src = "yahoo",from = start, to = end, auto.assign = F)[,4]
}

getDailyReturn <- function(priceSeries){
  out<- dailyReturn(priceSeries)
  out<- out[2:nrow(out)]
  names(out)<-names(priceSeries)
  return(out)
}

retDfSum <- function(retDF){
  apply(retDF, 2, function(rets){prod(1+rets)-1})
}

#-----------------------------------------------------------

# IMPLEMENTING SMART-BETA

# Get prices for constituents 
symbols<- get_DJ_symbols()
allPrices <- map(symbols, getPriceSeries )
names(allPrices) <- symbols

# Compute returns from prices
dailyReturns <- map(allPrices, getDailyReturn)
dailyReturns <- Reduce(cbind, dailyReturns)
monthlyReturns <- apply.monthly(dailyReturns, retDfSum)

# Compute signal for Low-vol strategy: Rolling 3-month daily-vol
rolling_3mo_daily_vol <- apply.monthly(na.trim(rollapply(dailyReturns, 63, sd)), last)

# Compute signal for Momentum strategy: Rolling 12-month return
rolling_12mo_returns<- na.trim(rollapply(monthlyReturns, 12, retDfSum))

# Compute signal for Combination strategy: Rolling 12-month return/Rolling 3-month daily-vol
combination_signal <- rolling_12mo_returns/rolling_3mo_daily_vol

# MOMENTUM STRATEGY

wts_momentum <- t(apply(rolling_12mo_returns, 1, isTopN, n_momentum)) * (1/n_momentum)
wts_momentum <- xts(wts_momentum, order.by=as.Date(rownames(wts_momentum))) 
momentum_return_contributions <- na.trim(lag(wts_momentum, 1)) * monthlyReturns
momentum_returns <- apply(momentum_return_contributions, 1, sum)
momentum_returns <- xts(momentum_returns, order.by=as.Date(names(momentum_returns)))
names(momentum_returns) <- "momentum"

# LOW-VOL STRATEGY

wts_lowvol <- t(apply(rolling_3mo_daily_vol, 1, isBottomN, n_lowvol)) * (1/n_lowvol)
wts_lowvol <- xts(wts_lowvol, order.by=as.Date(rownames(wts_lowvol)))
lowvol_return_contributions <- na.trim(lag(wts_lowvol, 1)) * monthlyReturns
lowvol_returns <- apply(lowvol_return_contributions, 1, sum)
lowvol_returns <- xts(lowvol_returns, order.by=as.Date(names(lowvol_returns)))
names(lowvol_returns)<-"lowvol"

# COMBINATION STRATEGY

wts_combination <- t(apply(combination_signal, 1, isTopN, n_combination)) * (1/n_combination)
wts_combination <- xts(wts_combination, order.by=as.Date(rownames(wts_combination)))
combination_return_contributions <- na.trim(lag(wts_combination, 1)) * monthlyReturns
combination_returns <- apply(combination_return_contributions, 1, sum)
combination_returns <- xts(combination_returns, order.by=as.Date(names(combination_returns)))
names(combination_returns)<-"combination"

# DOW JONES INDEX

dji <- getSymbols("^DJI", from=start, to=end, auto.assign = F)[,4]
dji_monthly <- apply.monthly(dji, last)
dji_returns <- monthlyReturn(dji_monthly)
names(dji_returns)<-"djia"

# PLOT

strategies <- na.trim(cbind(momentum_returns, lowvol_returns, combination_returns, dji_returns))
cum_returns <- cumsum(strategies)
dummy_row <- do.call(cbind, map(1:4,function(x){xts(0, order.by = head(index(cum_returns),1)-months(1))}))
names(dummy_row) <- names(cum_returns)
plot.zoo( rbind(dummy_row,cum_returns), screens=1, col = c("red", "green", "blue", "black"), main="Cumulative Returns for different smart beta strategies 2012-2017", xlab = "", )
legend("topleft", legend=names(cum_returns), col = c("red", "green", "blue", "black"), lty=1 )
grid(col="black")
# TABLE

library(PerformanceAnalytics)
table.AnnualizedReturns(strategies)

