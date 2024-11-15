

#####################################################################################
################# 
#################         PAYROLLS x INFLATION
#################
#####################################################################################
payroll_inf <- cbind(payroll, inf) %>% 
  na.omit()     

adf.test(consumption)

### determine optimal lags ###
### optimal lags = 4
VARselect(payroll_inf)
### BUILD MODEL W 1 LAGS ###
payroll_inf_model <- VAR(payroll_inf, p=4, type="const")

### evaluate model ###
###
### SERIAL CORRELATION! FAILS PORTMANEAU TEST
### 
serial.test(payroll_inf_model)
stability(payroll_inf_model, type="OLS-CUSUM") %>% plot()

### GRANGER CAUSALITY & INSTANTANEOUS CAUSALITY
causality(payroll_inf_model, cause="inf") ### instantaneous NOT Granger 

###
### IMPULSE RESPONSE FUNCTION
###
library(vars)
payroll_inf_irf <- vars::irf(payroll_inf_model, impulse="inf", response="payroll", n.ahead=8, boot=T)
plot(payroll_inf_irf)

### EXTRACT EARLY DATA ###
payroll_inf_early <- window(payroll_inf, end=c(2020, 3))

### build model again and this time PREDICT out ###
### build model 1
### find optimal lag
VARselect(payroll_inf_early)

### build model
early_payroll_inf_model <- VAR(payroll_inf_early, p=10, type="const")

### evaluate model
serial.test(early_payroll_inf_model)
stability(early_payroll_inf_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
causality(early_payroll_inf_model, cause="inf") ### instan AND granger

### predict it
early_payroll_inf_pred <- predict(early_payroll_inf_model, n.ahead=13)

early_payroll_inf_forecast <- early_payroll_inf_pred$fcst$payroll[, c("upper", "lower", "fcst")]

###compare to actual rgdp
actual_payroll <- window(payroll, start=c(2020, 3), end=c(2023, 3))
payroll_inf_forecast <- cbind(early_payroll_inf_forecast, actual_payroll)

### TIMKETK DATAFRAME TO USE DATES ###
payroll_inf_forecast_df <- timetk::tk_tbl(payroll_inf_forecast)

### RGDP FORECAST W INTEREST RATES ###
#### COMPARE W ACTUAL RGDP OVER THE SAME TIME PERIOD ###
payroll_inf_prior 
ggplot(payroll_inf_forecast_df, aes(x = index, y = early_payroll_inf_forecast.fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = early_payroll_inf_forecast.lower, ymax = early_payroll_inf_forecast.upper), 
              fill = "blue", alpha = 0.2) +
  geom_point(aes(y = actual_payroll), color = "black", size = 2, shape = 16) +
  labs(title = "Conditional on Inflation", subtitle="Log Scale",
       x = "Quarter",
       y = "Jobs") 