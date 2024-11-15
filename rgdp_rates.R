

#####################################################################################
################# 
#################         RGDP x FUNDS RATE
#################
#####################################################################################
gdp_rate <- cbind(rgdp, funds_rate) %>% 
  na.omit()     

### determine optimal lags ###
### optimal lags = 4
VARselect(gdp_rate)
head(gdp_rate)
### BUILD MODEL W 1 LAGS ###
gdp_rate_model <- VAR(gdp_rate, p=6, type="const")

### evaluate model ###
###
### SERIAL CORRELATION! FAILS PORTMANEAU TEST
###
serial.test(gdp_rate_model)
stability(gdp_rate_model, type="OLS-CUSUM") %>% plot()

### GRANGER CAUSALITY & INSTANTANEOUS CAUSALITY
causality(gdp_rate_model, cause="funds_rate") ### instan & Granger cause

###
### IMPULSE RESPONSE FUNCTION
###
library(vars)
gdp_rate_irf <- vars::irf(gdp_rate_model, impulse="funds_rate", response="rgdp", n.ahead=8, boot=T)
par(mfrow=c(1,2))
plot(gdp_rate_irf, main="Interest Rate Shock on Real GDP", ylab="Real GDP % Change")
plot(cons_rate_irf, main="Interest Rate Shock on Consumption", ylab="Consumption % Change")


### EXTRACT EARLY DATA ###
gdp_rate_early <- window(gdp_rate, end=c(2020, 3))

### build model again and this time PREDICT out ###
### build model 1
### find optimal lag
VARselect(gdp_rate_early)

### build model
early_gdp_rate_model <- VAR(gdp_rate_early, p=6, type="const")

### evaluate model
serial.test(early_gdp_rate_model)
stability(early_gdp_rate_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
### YES BOTH GRANGER & INSTANTANEOUS CAUSALITY
causality(early_gdp_rate_model, cause="funds_rate") ### instan AND granger

### predict it
early_gdp_rate_pred <- predict(early_gdp_rate_model, n.ahead=13)

early_gdp_rate_forecast <- early_gdp_rate_pred$fcst$rgdp[, c("upper", "lower", "fcst")]

###compare to actual rgdp
rgdp_rate_forecast <- cbind(early_gdp_rate_forecast, actual_rgdp)

### TIMKETK DATAFRAME TO USE DATES ###
rgdp_rate_forecast_df <- timetk::tk_tbl(rgdp_rate_forecast)

### RGDP FORECAST W INTEREST RATES ###
#### COMPARE W ACTUAL RGDP OVER THE SAME TIME PERIOD ###
rgdp_rate_prior <- ggplot(rgdp_rate_forecast_df, aes(x = index, y = early_gdp_rate_forecast.fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = early_gdp_rate_forecast.lower, ymax = early_gdp_rate_forecast.upper), 
              fill = "blue", alpha = 0.2) +
  geom_point(aes(y = actual_rgdp), color = "black", size = 2, shape = 16) +
  labs(title = "Conditional on Interest Rate", subtitle="Log Scale",
       x = "Quarter",
       y = "RGDP") 

###
### future predictions for rgdp x inf
###

### predict it
gdp_rate_pred <- predict(gdp_rate_model, n.ahead=5)

gdp_rate_forecast <- gdp_rate_pred$fcst$rgdp[, c("upper", "lower", "fcst")]

### TIMKETK DATAFRAME TO USE DATES ###
rgdp_rate_forecast_future <- timetk::tk_tbl(gdp_rate_forecast)

# Assuming you have a time series of dates for the forecasted values
dates_forecast <- c("2023 Q4", "2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4")

dates_forecast_as_date <- as.Date(as.yearqtr(dates_forecast, format = "%Y Q%q"))
### RGDP FORECAST W INTEREST RATES ###
#### COMPARE W ACTUAL RGDP OVER THE SAME TIME PERIOD ###
rgdp_m2_future 
ggplot(rgdp_rate_forecast_future, aes(x = dates_forecast_as_date, y = fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill = "blue", alpha = 0.2) +
  labs(title = "Conditional on Interest Rate",
       x = "Quarter",
       y = "RGDP") 
