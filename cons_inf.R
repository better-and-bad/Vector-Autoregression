
#####################################################################################
################# 
#################         Consumption x INFLATION
#################
#####################################################################################
cons_inf <- cbind(consumption, inf) %>% 
  na.omit()     

### determine optimal lags ###
### optimal lags = 4
VARselect(cons_inf)
### BUILD MODEL W 1 LAGS ###
cons_inf_model <- VAR(cons_inf, p=4, type="const")

### evaluate model ###
###
### SERIAL CORRELATION! FAILS PORTMANEAU TEST
###
serial.test(cons_inf_model)
stability(cons_inf_model, type="OLS-CUSUM") %>% plot()

### GRANGER CAUSALITY & INSTANTANEOUS CAUSALITY
causality(cons_inf_model, cause="inf") ### instantaneous NOT Granger 

###
### IMPULSE RESPONSE FUNCTION
###
library(vars)
cons_inf_irf <- vars::irf(cons_inf_model, impulse="inf", response="consumption", n.ahead=8, boot=T)
plot(cons_inf_irf)

### EXTRACT EARLY DATA ###
cons_inf_early <- window(cons_inf, end=c(2020, 3))

### build model again and this time PREDICT out ###
### build model 1
### find optimal lag
VARselect(cons_inf_early)

### build model
early_cons_inf_model <- VAR(cons_inf_early, p=4, type="const")

### evaluate model
serial.test(early_cons_inf_model)
stability(early_cons_inf_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
### YES BOTH GRANGER & INSTANTANEOUS CAUSALITY
causality(early_cons_inf_model, cause="inf") ### instan but not granger

### predict it
early_cons_inf_pred <- predict(early_cons_inf_model, n.ahead=13)

early_cons_inf_forecast <- early_cons_inf_pred$fcst$consumption[, c("upper", "lower", "fcst")]

###compare to actual rgdp
actual_consumption <- window(consumption, start=c(2020, 3), end=c(2023, 3))
cons_inf_forecast <- cbind(early_cons_inf_forecast, actual_consumption)

### TIMKETK DATAFRAME TO USE DATES ###
cons_inf_forecast_df <- timetk::tk_tbl(cons_inf_forecast)

### RGDP FORECAST W INTEREST RATES ###
#### COMPARE W ACTUAL RGDP OVER THE SAME TIME PERIOD ###
cons_inf_prior <- ggplot(cons_inf_forecast_df, aes(x = index, y = early_cons_inf_forecast.fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = early_cons_inf_forecast.lower, ymax = early_cons_inf_forecast.upper), 
              fill = "blue", alpha = 0.2) +
  geom_point(aes(y = actual_consumption), color = "black", size = 2, shape = 16) +
  labs(title = "Conditional on Inflation", subtitle="Log Scale",
       x = "Quarter",
       y = "Consumption") 
