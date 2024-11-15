#####################################################################################
################# 
#################         RGDP x INFLATION
#################
#####################################################################################
gdp_inf <- cbind(rgdp, inf) %>% 
  na.omit()     

### determine optimal lags ###
### optimal lags = 4
VARselect(gdp_inf)
head(gdp_inf)
### BUILD MODEL W 1 LAGS ###
gdp_inf_model <- VAR(gdp_inf, p=4, type="const")

### evaluate model ###
###
### SERIAL CORRELATION! FAILS PORTMANEAU TEST
###
serial.test(gdp_inf_model)
stability(gdp_inf_model, type="OLS-CUSUM") %>% plot()

### GRANGER CAUSALITY & INSTANTANEOUS CAUSALITY
causality(gdp_inf_model, cause="inf") ### inf doesn't granger cause 
### does instantaneous cause

###
### IMPULSE RESPONSE FUNCTION
###
library(vars)
gdp_inf_irf <- vars::irf(gdp_inf_model, impulse="inf", response="rgdp", n.ahead=8, boot=T)
plot(gdp_inf_irf)

### EXTRACT EARLY DATA ###
gdp_inf_early <- window(gdp_inf, end=c(2020, 3))

### build model again and this time PREDICT out ###
### build model 1
### find optimal lag
VARselect(gdp_inf_early)

### build model
early_gdp_inf_model <- VAR(gdp_inf_early, p=6, type="const")

### evaluate model
serial.test(early_gdp_inf_model)
stability(early_gdp_inf_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
### YES BOTH GRANGER & INSTANTANEOUS CAUSALITY
causality(early_gdp_inf_model, cause="inf") ### instan but not granger

### predict it
early_gdp_inf_pred <- predict(early_gdp_inf_model, n.ahead=13)

early_gdp_inf_forecast <- early_gdp_inf_pred$fcst$rgdp[, c("upper", "lower", "fcst")]

###compare to actual rgdp
rgdp_inf_forecast <- cbind(early_gdp_inf_forecast, actual_rgdp)

### TIMKETK DATAFRAME TO USE DATES ###
rgdp_inf_forecast_df <- timetk::tk_tbl(rgdp_inf_forecast)

### RGDP FORECAST W INTEREST RATES ###
#### COMPARE W ACTUAL RGDP OVER THE SAME TIME PERIOD ###
rgdp_inf_prior <- ggplot(rgdp_inf_forecast_df, aes(x = index, y = early_gdp_inf_forecast.fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = early_gdp_inf_forecast.lower, ymax = early_gdp_inf_forecast.upper), 
              fill = "blue", alpha = 0.2) +
  geom_point(aes(y = actual_rgdp), color = "black", size = 2, shape = 16) +
  labs(title = "Conditional on Inflation", subtitle="Log Scale",
       x = "Quarter",
       y = "RGDP") 

###
### future predictions for rgdp x inf
###

### predict it
gdp_inf_pred <- predict(gdp_inf_model, n.ahead=5)

gdp_inf_forecast <- gdp_inf_pred$fcst$rgdp[, c("upper", "lower", "fcst")]

### TIMKETK DATAFRAME TO USE DATES ###
rgdp_inf_forecast_future <- timetk::tk_tbl(gdp_inf_forecast)

# Assuming you have a time series of dates for the forecasted values
dates_forecast <- c("2023 Q4", "2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4")

dates_forecast_as_date <- as.Date(as.yearqtr(dates_forecast, format = "%Y Q%q"))
### RGDP FORECAST W INTEREST RATES ###
#### COMPARE W ACTUAL RGDP OVER THE SAME TIME PERIOD ###
rgdp_m2_future 
ggplot(rgdp_inf_forecast_future, aes(x = dates_forecast_as_date, y = fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill = "blue", alpha = 0.2) +
  labs(title = "Conditional on Inflation",
       x = "Quarter",
       y = "RGDP") 