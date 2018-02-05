

# Helper function to align data index between different data sources
fixDates <- function(dates) {
  yr <- format(dates, '%Y')
  mo <- format(dates, '%m')
  day <- '01'
  
  as.Date(paste0(yr, "-", mo, "-", day))
}

library(xts)
library(lubridate)
library(Quandl)

# RESPONSE VARIABLE: Real GDP quarterly growth (annualized)

# PREDICTOR VARIABLE 1: investor sentiment measured as spread between percentage of investors
# bullish and percentage of investors bearish, normalized by percent of investors that had a non-neutral
# view, i.e., were either bullish or bearish

# PREDICTOR VARIABLE 2: consumer sentiment, as measured by University of Michigan surveys

# Download data for response variable

GDP <- Quandl('FRED/A191RL1Q225SBEA') # quarterly series
GDP <- xts(GDP$Value, order.by = GDP$Date)
# make index correspond to quarter-ending month rather than quarter-beginning month
index(GDP) <- index(GDP) + months(2) 

# Download data for predictor variables

AAII <- Quandl('AAII/AAII_SENTIMENT') # weekly series
investor_sentiment_weekly <- na.trim(xts(AAII$`Bull-Bear Spread`/(1-AAII$Neutral), order.by=AAII$Date))
investor_sentiment_quarterly <- apply.quarterly(investor_sentiment_weekly, function(x){mean(x, na.rm=T)})
# to align with response, make index correspond to first day of quarter-ending month
index(investor_sentiment_quarterly) <- fixDates(index(investor_sentiment_quarterly))

MICH <- Quandl('UMICH/SOC1') # monthly series 
consumer_sentiment <- xts(MICH$Index, order.by=MICH$Date)
consumer_sentiment_quarterly <- apply.quarterly(consumer_sentiment, function(x){mean(x, na.rm=T)})
index(consumer_sentiment_quarterly) <- fixDates(index(consumer_sentiment_quarterly))

data_gdp <- merge(merge(GDP, consumer_sentiment_quarterly, join = 'inner'), 
                  investor_sentiment_quarterly, join = 'inner')

# It is reasonable to expect that high investor and consumer sentiment today would be reflected
# in increased real gdp growth a quarter from now. We use lagged predictors for forecasting response.

investor_sentiment_quarterold <- lag(investor_sentiment_quarterly)
consumer_sentiment_quarterold <- lag(consumer_sentiment_quarterly)

data_gdp <- na.trim(merge(merge(data_gdp, consumer_sentiment_quarterold, join='inner'), 
                          investor_sentiment_quarterold, join='inner'))

mod <- lm(GDP ~ consumer_sentiment_quarterold + investor_sentiment_quarterold, data=data_gdp)
summary(mod)

# Residuals vs fitted values
plot(mod$fitted.values, mod$residuals, xlab="Fitted Values", ylab ="Residuals", main="Residuals vs Fitted values")

# Residuals don't look probelmatic. To be safe, check for stationarity of residuals using adf test.
adf.test(mod$residuals, alternative="s")
# Strong evidence to reject the nonstationary null hypothesis.

# Response vs fitted values
plot(mod$fitted.values + mod$residuals, mod$fitted.values, xlab="Response", ylab="Fitted values", 
     main=paste0("Fitted Values vs Response | R.Sq. ", round(summary(model)$r.squared,2)))









