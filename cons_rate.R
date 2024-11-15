


#####################################################################################
################# 
#################         CONSUMPTION x FUNDS RATE
#################
#####################################################################################
cons_rate <- cbind(consumption, funds_rate) %>% 
  na.omit()     

### determine optimal lags ###
### optimal lags = 4
VARselect(cons_rate)
### BUILD MODEL W 1 LAGS ###
cons_rate_model <- VAR(cons_rate, p=8, type="const")

### evaluate model ###
###
### SERIAL CORRELATION! FAILS PORTMANEAU TEST
###
serial.test(cons_rate_model)
stability(cons_rate_model, type="OLS-CUSUM") %>% plot()

### GRANGER CAUSALITY & INSTANTANEOUS CAUSALITY
causality(cons_rate_model, cause="funds_rate") ### instantaneous NOT Granger 

###
### IMPULSE RESPONSE FUNCTION
###
library(vars)
cons_rate_irf <- vars::irf(cons_rate_model, impulse="funds_rate", response="consumption", n.ahead=8, boot=T)
plot(cons_rate_irf, main="Interest Rate Shock on Consumption", ylab="Consumption % Change")

### EXTRACT EARLY DATA ###
cons_rate_early <- window(cons_rate, end=c(2020, 3))

### build model again and this time PREDICT out ###
### build model 1
### find optimal lag
VARselect(cons_rate_early)

### build model
early_cons_rate_model <- VAR(cons_rate_early, p=6, type="const")

### evaluate model
serial.test(early_cons_rate_model)
stability(early_cons_rate_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
### YES BOTH GRANGER & INSTANTANEOUS CAUSALITY
causality(early_cons_rate_model, cause="funds_rate") ### instan but not granger

### predict it
early_cons_rate_pred <- predict(early_cons_rate_model, n.ahead=13)

early_cons_rate_forecast <- early_cons_rate_pred$fcst$consumption[, c("upper", "lower", "fcst")]

###compare to actual rgdp
actual_consumption <- window(consumption, start=c(2020, 3), end=c(2023, 3))
early_cons_rate_forecast <- cbind(early_cons_rate_forecast, actual_consumption)

### TIMKETK DATAFRAME TO USE DATES ###
cons_rate_forecast_df <- timetk::tk_tbl(early_cons_rate_forecast)

### RGDP FORECAST W INTEREST RATES ###
#### COMPARE W ACTUAL RGDP OVER THE SAME TIME PERIOD ###
cons_rate_prior <- ggplot(cons_rate_forecast_df, aes(x = index, y = early_cons_rate_forecast.fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = early_cons_rate_forecast.lower, ymax = early_cons_rate_forecast.upper), 
              fill = "blue", alpha = 0.2) +
  geom_point(aes(y = actual_consumption), color = "black", size = 2, shape = 16) +
  labs(title = "Conditional on Interest Rate", subtitle="Log Scale",
       x = "Quarter",
       y = "Consumption") 
