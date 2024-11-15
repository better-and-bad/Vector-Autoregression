

#####################################################################################
################# 
#################         UNEMPLOYMENT x INFLATION
#################
#####################################################################################
unemploy_inf <- cbind(unemploy, inf) %>% 
  na.omit()     

### determine optimal lags ###
### optimal lags = 4
VARselect(unemploy_inf)
### BUILD MODEL W 1 LAGS ###
unemploy_inf_model <- VAR(unemploy_inf, p=4, type="const")

### evaluate model ###
###
### SERIAL CORRELATION! FAILS PORTMANEAU TEST
### 
serial.test(unemploy_inf_model)
stability(unemploy_inf_model, type="OLS-CUSUM") %>% plot()

### GRANGER CAUSALITY & INSTANTANEOUS CAUSALITY
causality(unemploy_inf_model, cause="inf") ### instantaneous NOT Granger 

###
### IMPULSE RESPONSE FUNCTION
###
library(vars)
unemploy_inf_irf <- vars::irf(unemploy_inf_model, impulse="inf", response="unemploy", n.ahead=8, boot=T)
plot(unemploy_inf_irf)

### EXTRACT EARLY DATA ###
unemploy_inf_early <- window(unemploy_inf, end=c(2020, 3))

### build model again and this time PREDICT out ###
### build model 1
### find optimal lag
VARselect(unemploy_inf_early)

### build model
early_unemploy_inf_model <- VAR(unemploy_inf_early, p=4, type="const")

### evaluate model
serial.test(early_unemploy_inf_model)
stability(early_unemploy_inf_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
causality(early_unemploy_inf_model, cause="inf") ### instan AND granger

### predict it
early_unemploy_inf_pred <- predict(early_unemploy_inf_model, n.ahead=13)

early_unemploy_inf_forecast <- early_unemploy_inf_pred$fcst$unemploy[, c("upper", "lower", "fcst")]

###compare to actual rgdp
actual_unemploy <- window(unemploy, start=c(2020, 3), end=c(2023, 3))
unemploy_inf_forecast <- cbind(early_unemploy_inf_forecast, actual_unemploy)

### TIMKETK DATAFRAME TO USE DATES ###
unemploy_inf_forecast_df <- timetk::tk_tbl(unemploy_inf_forecast)

### RGDP FORECAST W INTEREST RATES ###
#### COMPARE W ACTUAL RGDP OVER THE SAME TIME PERIOD ###
unemploy_inf_prior <- ggplot(unemploy_inf_forecast_df, aes(x = index, y = early_unemploy_inf_forecast.fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = early_unemploy_inf_forecast.lower, ymax = early_unemploy_inf_forecast.upper), 
              fill = "blue", alpha = 0.2) +
  geom_point(aes(y = actual_unemploy), color = "black", size = 2, shape = 16) +
  labs(title = "Conditional on Inflation", subtitle="Log Scale",
       x = "Quarter",
       y = "Unemployment") 

###
### future predictions for rgdp x inf
###

### predict it
unemploy_inf_pred <- predict(unemploy_inf_model, n.ahead=5)

unemploy_inf_forecast <- unemploy_inf_pred$fcst$unemploy[, c("upper", "lower", "fcst")]

### TIMKETK DATAFRAME TO USE DATES ###
unemploy_inf_forecast_future <- timetk::tk_tbl(unemploy_inf_forecast)

# Assuming you have a time series of dates for the forecasted values
dates_forecast <- c("2023 Q4", "2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4")

dates_forecast_as_date <- as.Date(as.yearqtr(dates_forecast, format = "%Y Q%q"))
### RGDP FORECAST W INTEREST RATES ###
#### COMPARE W ACTUAL RGDP OVER THE SAME TIME PERIOD ###
cons_inf_future 
ggplot(unemploy_inf_forecast_future, aes(x = dates_forecast_as_date, y = fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill = "blue", alpha = 0.2) +
  labs(title = "Conditional on Inflation",
       x = "Quarter",
       y = "RGDP") +
  theme_minimal()