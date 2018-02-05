# predict_fx

#a) Suggest one data source that might be useful to explain or predict the FX market.
#b) Derive and discuss relevant analytics from this data source.
#c) Determine whether your proposed analytics are co-integrated with currency pairs.
#d) Describe and implement a pairs trading strategy exploiting your analytics.

# PREDICTOR 1: OIL PRICE
# Economies that are heavily reliant on oil exports see large swings in their currency's value driven by the price of oil
# With this hypotheis, it may be argued that the currency of Norway, Krone, could be predicted using the oil price
# https://www.weforum.org/agenda/2016/05/which-economies-are-most-reliant-on-oil/

# PREDICTOR 2: INTEREST RATE DIFFERENTIAL
# Due to carry traders, in the short to medium term, currencies with higher interest rates tend to get stronger
# due to investor interest.

# RESPONSE VARIABLE: NORWEGIAN KRONE FX RATE

usdInKrones <- Quandl('FRED/DEXNOUS')
usdInKrones <- xts(usdInKrones$Value, usdInKrones$Date)
usdInKrones <- apply.monthly(usdInKrones, last)
index(usdInKrones) <- as.yearmon(index(usdInKrones))
head(usdInKrones)

# PREDICTOR 1: OIL PRICE

oil_continuous <- xts(Quandl('CHRIS/ICE_B1')$Settle, Quandl('CHRIS/ICE_B11')$Date)
oil_price_monthly<-apply.monthly(oil_continuous, last)
index(oil_price_monthly) <- as.yearmon(index(oil_price_monthly))
head(oil_price_monthly)

# PREDICTOR 2: INTEREST RATE DIFFERENTIAL

norway_rates_df <- read.csv('https://www.norges-bank.no/globalassets/marketdata/stat/en/renter/v2/renter_mnd.csv')
library(lubridate)
dates<-dmy(paste0('01-',as.character(norway_rates_df$DATES)))
norway_rates <- xts(norway_rates_df[,2:ncol(norway_rates_df)], order.by=dates)
norway_rf <- norway_rates$STATSVKL.3M.EFF
norway_rf <- norway_rf[norway_rf[,1]!="ND"]
norway_rf <- xts(as.numeric(norway_rf), index(norway_rf))
index(norway_rf) <- as.yearmon(index(norway_rf))
month_end_norway_rf <- apply.monthly(norway_rf, last)
index(month_end_norway_rf) <- as.yearmon(index(month_end_norway_rf))

us_rates <- Quandl('FRED/TB3MS')
us_rates <- xts(us_rates$Value, order.by=us_rates$Date)
month_end_us_rf <- apply.monthly(us_rates, last)
index(month_end_us_rf) <- as.yearmon(index(month_end_us_rf))

IRD <- month_end_us_rf - month_end_norway_rf
names(IRD) <- "IRD"

# We expect a positive coeff to IRD and neg coeff to oil price

data_fx2 <- na.trim(merge(merge(usdInKrones, oil_price_monthly), IRD))
model <- lm(usdInKrones ~  oil_price_monthly + IRD, data=data_fx2)
summary(model)

# The signs of model coefficients are in line with our expectation

# Diagnostics
plot(model$fitted.values, model$residuals)
# The residuals vs fitted values chart indicates greater vol for residuals for bigger fitted values 
# (log transform  may help the model - but will render it untradable)
# check if the residuals are stationary
adf.test(model$residuals) # as expected, residuals don't show strong evidence for stationarity

# since the residuals are hardly stationary, the regression does not define a cointegrated combination of 
# the predictors with the fx rate. Therefore, we do not have a strong case for pairs trading.


#---------------------------------------------------------------------------------------------
# Implementing pairs trading using (FX_return ~ Oil_price + Interest_rate_differential) model
#---------------------------------------------------------------------------------------------

# Pairs trading rule
coeff <- model$coefficients

#  wts as obtained as per regression above
# USD/Krone = Buy 1 contract
# Oil futures = Buy -coeff[1] contracts
# USTsy-NorwayTsyRrate = Buy -coeff[2] contracts, where buying 1 contract means = Buy 1 ITM US tsy 3-mo cap, sell 1 ITM Norway 3mo cap
cointegrated_wts <- c(1, -coeff[2], -coeff[3])
# These wts correspond to unit long position in the stationary process obtained from regression.
# Buying in these proportions will signify a bet on the mean-reversion of the stationary residual

SD <-sd(model$residuals)
intercept <- coeff[1]
zero_weights <- c(0, 0, 0)
wts<-list()
wts[[1]] <- zero_weights
current_position = 'no'

for (i in 1:nrow(data_fx2)) {
    
     stationary_process_value <- sum(data_fx2[i,] * cointegrated_wts) - intercept
     
     if(current_position == 'no') {
     if(stationary_process_value > 1.5*SD & stationary_process_value <= 2.5*SD){
       # go short the stationary process
       wts[[i]] = -1 * cointegrated_wts
       current_position = 'short'
     }
     if(stationary_process_value < -1.5*SD & stationary_process_value >= -2.5*SD){
       # go long the stationary process
       wts[[i]] = cointegrated_wts
       current_position = 'long'
     }
     if(stationary_process_value <= 1.5*SD & stationary_process_value >= -1.5*SD) {
       # do nothing
       wts[[i]]= zero_weights
     }
     if(stationary_process_value > 2.5*SD | stationary_process_value < -2.5*SD){
       # do nothing
       wts[[i]] = zero_weights
     }
     }
     
     
     if(current_position == 'short') {
       if(stationary_process_value <= 0.3*SD & stationary_process_value >= -1.5*SD ){
         # close the position
         wts[[i]] <- zero_weights
         current_position = 'no'
       }
       if(stationary_process_value < -1.5*SD & stationary_process_value > -2.5*SD){
         # close the position and
         # go long the stationary process
         wts[[i]] <- cointegrated_wts
         current_position = 'long'
       }
       if(stationary_process_value > 2.5*SD){
         # close the position - cut your losses
         wts[[i]] <- zero_weights
         current_position = 'no'
       }
       if(stationary_process_value < -2.5*SD){
         # close the position - something strange is going on so don't initiate a long position
         wts[[i]] <- zero_weights
         current_position = 'no'
       }
       if(stationary_process_value <= 2.5*SD & stationary_process_value > 0.3*SD){
         # continue as before
         wts[[i]] <- wts[[i-1]]
         
       }
     }
     
     
     
     if(current_position == 'long') {
       if(stationary_process_value >= -0.3*SD & stationary_process_value <= 1.5*SD ){
         # close the position 
         wts[[i]] <- zero_weights
       }
       if(stationary_process_value > 1.5*SD & stationary_process_value <= 2.5*SD){
         # close the position
         # go short the stationary process
         wts[[i]] <- -1*cointegrated_wts
       }
       if(stationary_process_value < -2.5*SD){
         # close the position - cut your losses
         wts[[i]] <- zero_weights
       }
       if(stationary_process_value > 2.5*SD){
         # close the position - something strange is going on so don't initiate a short position
         wts[[i]] <- zero_weights
       }
       if(stationary_process_value >= -2.5*SD & stationary_process_value < -0.3*SD){
         # continue as before
         wts[[i]] <- wts[[i-1]]
       }
     }
     
     

  }  

weights <- do.call(rbind, wts)
price_moves <- na.trim(diff(data_fx2))
PNL_contribs <- head(weights, -1) * coredata(price_moves)
PNL_contribs <- xts(PNL_contribs, order.by=index(price_moves))
PNL <- apply(PNL_contribs, 1, sum)
PNL <- xts(PNL, index(price_moves))
cumulative_PNL <- cumsum(PNL)
plot(cumulative_PNL)



