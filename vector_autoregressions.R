

rm(list=ls())

library(tidyverse)
library(fredr)
library(tsibble)
library(fpp3)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(vars)

library(BVAR)
### SET PLOT THEME ###
theme_cx <- function(){
  theme_light() +
    theme(
      axis.text = element_text(size=16),
      axis.title.x= element_text(size=12, margin = margin(t=12)),
      axis.title.y= element_text(size=18, margin = margin(r=12)),
      panel.grid.minor = element_blank(),
      plot.margin = margin(1, 1, 1, unit="cm"),
      plot.title = element_text(size=16, face="bold", margin = margin(b=10),
                                hjust=0.5),
      plot.subtitle = element_text(size=14, face="bold", margin = margin(b=10),
                                hjust=0.5))
}

### set the theme
theme_set(theme_cx())

### Set Key
fredr_set_key("8454091b420f6979c70e84de8e611118")

### create df of vars
fred <- BVAR::fred_qd[c('CE16OV', "PAYEMS", "GDPC1", "PCECC96", "GDPCTPI","FEDFUNDS")]

### transform necessary variables
x_log <- fred_transform(fred, codes = c(4,4,4,4,4,1))

adf.test(x_log$PAYEMS)
### create prior settings
mn <- bv_minnesota(lambda = bv_lambda(mode=0.2, sd=0.4, min=0.0001, max=5),
                   alpha = bv_alpha(mode=2), var = 1e07)

### single root
soc <- bv_soc(mode=1, sd=1, min=1e-04, max=50)
sur <- bv_sur(mode=1, sd=1, min=1e-04, max=50)

### now that priors are defined, we can feed them into the priors
priors <- bv_priors(hyper = "auto", mn=mn, soc=soc, sur=sur)

### fine tune the posterior space
mh <- bv_metropolis(scale_hess = c(0.05, 0.0001, 0.0001),
                    adjust_acc=T, acc_lower=0.25, acc_upper = 0.45)

### ready for estimation / optimization
run <- bvar(x_log, lags=5, n_draw=15000, n_burn = 5000, n_thin=1,
            priors=priors, mh=mh,verbose=T)

### plot
plot(run) 
plot(run, type="dens", vars_response="GDPC1", 
     vars_impulse="GDPC1-lag1")

### plot residuals
plot(residuals(run))

### impulse response function 
x_irf <- bv_irf(horizon=16, identification = T)

irf(run) <- irf(run, x_irf, conf_bands=c(0.05, 0.16))

plot(irf(run), area=T, vars_impulse=c("GDPCTPI","FEDFUNDS"),
     vars_response=c(1:2), 6)

plot(irf(run), area = TRUE, vars_impulse = c("GDPCTPI", "FEDFUNDS"),
     vars_response = c("GDPC1", "PAYEMS", "CE16OV", "PCECC96"), forecast_horizon)
             
                   #### CHAT GPT ####
### extract predictions and compare them to actual values
# Assuming you have already run the IRF analysis
x_irf <- bv_irf(horizon = forecast_horizon, identification = TRUE)
irf(run) <- irf(run, x_irf, conf_bands = c(0.05, 0.16))

irf_output <- irf(run)

# Extract IRF predictions
irf_predictions <- irf_output$irf

# Specify the variable index (e.g., 1 for "GDPC1")
variable_index <- 1

# Extract IRF predictions for the specified variable and all time periods
irf_predictions <- irf_output$irf[, , variable_index, ]

# Assuming your original data is stored in 'x_log', and you have the actual values for the response variables
actual_values <- x_log[(nrow(x_log) - ncol(irf_predictions) + 1):nrow(x_log), variable_index]


# Plotting the comparison
matplot(1:ncol(irf_predictions), cbind(irf_predictions, actual_values), type = "l", lty = 1,
        col = c(rep(1, ncol(irf_predictions)), rep(2, ncol(actual_values))),
        xlab = "Time", ylab = "Variable", main = "IRF Predictions vs Actual Values")

# Adding legend
legend("topright", legend = c(paste("Prediction ", 1:ncol(irf_predictions)), "Actual Values"),
       col = c(rep(1, ncol(irf_predictions)), rep(2, ncol(actual_values))), lty = 1)


###

### LOAD DATA SETS ###
rgdp <- fredr(series_id="GDPC1") %>% 
  rename(rgdp = value) %>% 
  dplyr::select(-series_id, -realtime_start, -realtime_end) %>% 
  mutate(log_rgdp = log(rgdp))

pce <- fredr(series_id="PCE") %>% 
  rename(pce = value) %>% 
  dplyr::select(-series_id, -realtime_start, -realtime_end) %>% 
  mutate(log_pce = log(pce))

funds_rate <- fredr(series_id="FEDFUNDS") %>% 
  rename(rate = value) %>% 
  dplyr::select(-series_id, -realtime_start, -realtime_end)

payroll <- fredr(series_id="ADPWNUSNERSA") %>% 
  rename(jobs = value) %>% 
  dplyr::select(-series_id, -realtime_start, -realtime_end) %>% 
  mutate(log_jobs = log(jobs))

unemploy <- fredr(series_id="UNRATE") %>% 
  rename(unemploy = value) %>% 
  dplyr::select(-series_id, -realtime_start, -realtime_end) %>% 
  mutate(log_unemploy = log(unemploy))

inf <- fredr(series_id="DPCERD3Q086SBEA") %>% 
  rename(inf = value) %>% 
  dplyr::select(-series_id, -realtime_start, -realtime_end) %>% 
  mutate(log_inf = log(inf))

m2 <- fredr(series_id="WM2NS") %>% 
  rename(m2 = value) %>% 
  dplyr::select(-series_id, -realtime_start, -realtime_end) %>% 
  mutate(log_m2 = log(m2))



### aggregate the data so we can have a uniform frequency
rgdp <- rgdp %>% 
  mutate(year = zoo::as.yearqtr(date)) %>% 
  group_by(year) %>% 
  reframe(log_rgdp = mean(log_rgdp)) 
  #mutate(rgdp_change = ((rgdp - lag(rgdp))/lag(rgdp))*100) %>% 
  #na.omit()

m2 <- m2 %>%
  mutate(year = zoo::as.yearqtr(date)) %>%
  group_by(year) %>% 
  reframe(log_m2 = mean(log_m2))

consumption <- pce %>% 
  mutate(year = zoo::as.yearqtr(date)) %>% 
  group_by(year) %>% 
  reframe(log_pce = mean(log_pce)) 

funds_rate <- funds_rate %>%    
  mutate(year = zoo::as.yearqtr(date)) %>% 
  group_by(year) %>% 
  reframe(funds_rate = mean(rate))
 # mutate(rate_change = (interest_rate - lag(interest_rate))) %>% 
#  na.omit()

payroll <- payroll %>%    
    mutate(year = zoo::as.yearqtr(date)) %>% 
    group_by(year) %>% 
    reframe(log_jobs = mean(log_jobs)) 

unemploy <- unemploy %>%    
  mutate(year = zoo::as.yearqtr(date)) %>% 
  group_by(year) %>% 
  reframe(log_unemploy = mean(log_unemploy)) 

inf <- inf %>%    
  mutate(year = zoo::as.yearqtr(date)) %>% 
  group_by(year) %>% 
  reframe(log_inf = mean(log_inf)) 

###
### DEFINE TIME SERIES
### 
library(tseries)
rgdp <- ts(rgdp$log_rgdp, start=c(1947, 1), frequency=4)
consumption <- ts(consumption$log_pce, start=c(1959,1), frequency=4)
funds_rate <- ts(funds_rate$funds_rate, start=c(1954, 3), frequency=4)
payroll <- ts(payroll$log_jobs, start=c(2010, 1), frequency=4)
unemploy <- ts(unemploy$log_unemploy, start=c(1948, 1), frequency=4)
inf <- ts(inf$log_inf, start=c(1947, 1), frequency=4)
m2 <- ts(m2$log_m2, start=c(1980, 4), frequency=4)


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
  
  #####################################################################################
################# 
#################         UNEMPLOYMENT x FUNDS RATE
#################
#####################################################################################
unemploy_rate <- cbind(unemploy, funds_rate) %>% 
  na.omit()     

### determine optimal lags ###
### optimal lags = 4
VARselect(unemploy_rate)
### BUILD MODEL W 1 LAGS ###
unemploy_rate_model <- VAR(unemploy_rate, p=4, type="const")

### evaluate model ###
###
### SERIAL CORRELATION! FAILS PORTMANEAU TEST
### 
serial.test(unemploy_rate_model) 
stability(unemploy_rate_model, type="OLS-CUSUM") %>% plot()

### GRANGER CAUSALITY & INSTANTANEOUS CAUSALITY
causality(unemploy_rate_model, cause="funds_rate") ### instantaneous AND Granger 

###
### IMPULSE RESPONSE FUNCTION
###
library(vars)
unemploy_rate_irf <- vars::irf(unemploy_rate_model, impulse="funds_rate", response="unemploy", n.ahead=8, boot=T)
unemploy_rate_irf_plot <- plot(unemploy_rate_irf, main="Interest Rate Shock to Unemployment", ylab="Unemployment % Change",
     xlab="Future Quarters")

### EXTRACT EARLY DATA ###
unemploy_rate_early <- window(unemploy_rate, end=c(2020, 3))

### build model again and this time PREDICT out ###
### build model 1
### find optimal lag
VARselect(unemploy_rate_early)

### build model
early_unemploy_rate_model <- VAR(unemploy_rate_early, p=8, type="const")

### evaluate model
serial.test(early_unemploy_rate_model)
stability(early_unemploy_rate_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
causality(early_unemploy_rate_model, cause="funds_rate") ### instan NOT granger

### predict it
early_unemploy_rate_pred <- predict(early_unemploy_rate_model, n.ahead=13)

early_unemploy_rate_forecast <- early_unemploy_rate_pred$fcst$unemploy[, c("upper", "lower", "fcst")]

###compare to actual rgdp
actual_unemploy <- window(unemploy, start=c(2020, 3), end=c(2023, 3))
unemploy_rate_forecast <- cbind(early_unemploy_rate_forecast, actual_unemploy)

### TIMKETK DATAFRAME TO USE DATES ###
unemploy_rate_forecast_df <- timetk::tk_tbl(unemploy_rate_forecast)

### RGDP FORECAST W INTEREST RATES ###
#### COMPARE W ACTUAL RGDP OVER THE SAME TIME PERIOD ###
unemploy_rate_prior <- ggplot(unemploy_rate_forecast_df, aes(x = index, y = early_unemploy_rate_forecast.fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = early_unemploy_rate_forecast.lower, ymax = early_unemploy_rate_forecast.upper), 
              fill = "blue", alpha = 0.2) +
  geom_point(aes(y = actual_unemploy), color = "black", size = 2, shape = 16) +
  labs(title = "Conditional on Interest Rate", subtitle="Log Scale",
       x = "Quarter",
       y = "Unemployment") 

library(cowplot)
###
### PLOT PREDICTIONS AGAINST ACTUAL
###
plot_grid(rgdp_inf_prior, rgdp_rate_prior, cons_inf_prior, cons_rate_prior, unemploy_inf_prior,
          unemploy_rate_prior,
          ncol = 2)

#### IRF
# Extract impulse response functions
# Assuming you have the IRF results stored in gdp_inf_irf, cons_inf_irf, unemploy_inf_irf

# Extract IRF values
library(ggplot2)

# unemploy_inf_irf
# Extract IRF values
unemploy_rate_vals <- unemploy_rate_irf$irf$funds_rate[, "unemploy"]

# Extract Lower Band values
lower_band <- unemploy_rate_irf$Lower$funds_rate[, "unemploy"]

# Extract Upper Band values
upper_band <- unemploy_rate_irf$Upper$funds_rate[, "unemploy"]

# Create a data frame for ggplot
unemploy_rate_df <- data.frame(Quarter = 1:length(unemploy_rate_vals), 
                 unemploy_rate_vals = unemploy_rate_vals,
                 lower_band = lower_band,
                 upper_band = upper_band)

# Plot with ggplot
unemploy_irf_ggplot <- ggplot(unemploy_rate_df, aes(x = Quarter)) +
  geom_line(aes(y = unemploy_rate_vals), color = "blue", size = 1, linetype = "solid") +
  geom_ribbon(aes(ymin = lower_band, ymax = upper_band), fill = "blue", alpha = 0.2) +
  geom_hline(yintercept = 0, color="red") +
  labs(title = "Monetary Shock on Unemployment",
       x = "Future Quarters",
       y = "Unemploy. % Change") 

###
#### RGDP IRF GGPLOT 
###
# Extract IRF values
gdp_rate_vals <- gdp_rate_irf$irf$funds_rate[, "rgdp"]

# Extract Lower Band values
lower_band <- gdp_rate_irf$Lower$funds_rate[, "rgdp"]

# Extract Upper Band values
upper_band <- gdp_rate_irf$Upper$funds_rate[, "rgdp"]

# Create a data frame for ggplot
rgdp_irf_df <- data.frame(Quarter = 1:length(gdp_rate_vals), 
                              gdp_rate_vals = gdp_rate_vals,
                              lower_band = lower_band,
                              upper_band = upper_band)

# Plot with ggplot
rgdp_irf_ggplot <- ggplot(rgdp_irf_df, aes(x = Quarter)) +
  geom_line(aes(y = gdp_rate_vals), color = "blue", size = 1, linetype = "solid") +
  geom_ribbon(aes(ymin = lower_band, ymax = upper_band), fill = "blue", alpha = 0.2) +
  geom_hline(yintercept = 0, color="red") +
  labs(title = "Monetary Shock on Real GDP",
       x = "Future Quarters",
       y = "RGDP % Change") 
###
#### CONSUMPTION IRF GGPLOT 
###
# Extract IRF values
cons_rate_vals <- cons_rate_irf$irf$funds_rate[, "consumption"]

# Extract Lower Band values
lower_band <- cons_rate_irf$Lower$funds_rate[, "consumption"]

# Extract Upper Band values
upper_band <- cons_rate_irf$Upper$funds_rate[, "consumption"]

# Create a data frame for ggplot
cons_irf_df <- data.frame(Quarter = 1:length(cons_rate_vals), 
                          cons_rate_vals = cons_rate_vals,
                          lower_band = lower_band,
                          upper_band = upper_band)

# Plot with ggplot
cons_irf_ggplot <- ggplot(cons_irf_df, aes(x = Quarter)) +
  geom_line(aes(y = gdp_rate_vals), color = "blue", size = 1, linetype = "solid") +
  geom_ribbon(aes(ymin = lower_band, ymax = upper_band), fill = "blue", alpha = 0.2) +
  geom_hline(yintercept = 0, color="red") +
  labs(title = "Monetary Shock on Consumption",
       x = "Future Quarters",
       y = "Cons. % Change") 

library(cowplot)
plot_grid(rgdp_irf_ggplot, unemploy_irf_ggplot, cons_irf_ggplot, nrow=3)


#####
       ######## END ANALYSIS #########
#####

### var analysis
VARselect(log_gdp_m2) ### 8 lags best

### model
log_m2_gdp_model <- VAR(log_gdp_m2, p=8, type='const', season = NULL, exog=NULL)

summary(log_ms_gdp_model)

### test for stationarity
serial1 <- serial.test(log_m2_gdp_model, lags.pt=12)   ### stationary
stability(log_m2_gdp_model, type="OLS-CUSUM") %>% plot()

### are they granger causal
causality(log_m2_gdp_model, cause="log_m2"). ### both correlations yes

### impulse response function
library(vars)
log_m2_gdp_irf <- vars::irf(log_m2_gdp_model, impulse="log_m2", response="log_gdp", n.ahead=8, boot=T)
plot(log_m2_gdp_irf, ylab="RGDP", main="Shock from Money Base") ### gdp increases only after 1.5 years
                                                  ### does this lend credibility to monetarists ?

early_data <- window(log_gdp_m2, end=c(2020, 3))

VARselect(early_data, lag.max=10, type='const')

early_model <- VAR(early_data, p=4, type="const")

early_causality <- causality(early_model, cause="log_m2")

early_preds <- predict(early_model, n.ahead = 13)
fanchart(early_preds, names="rgdp", main="RGDP Forecast")

### extract predicted values
predicted <- early_preds$fcst$log_gdp[, "fcst"]

rgdp_preds <- early_preds$fcst$log_gdp[, c("upper", "lower", "fcst")]

actual_rgdp <-  window(log_rgdp, start= c(2020, 3), end = c(2023,3))
actual_rgdp <-  window(rgdp, start= c(2020, 3), end = c(2023,3))

rgdp_pred_forecast <- cbind(rgdp_preds, actual_rgdp)

### TIMKETK DATAFRAME TO USE DATES ###
rgdp_pred_forecast_df <- timetk::tk_tbl(rgdp_pred_forecast)

### RGDP FORECAST W INTEREST RATES ###
#### COMPARE W ACTUAL RGDP OVER THE SAME TIME PERIOD ###
log_gdpxlog_m2 <- ggplot(rgdp_pred_forecast_df, aes(x = index, y = rgdp_preds.fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = rgdp_preds.lower, ymax = rgdp_preds.upper), 
              fill = "blue", alpha = 0.2) +
  geom_point(aes(y = actual_rgdp), color = "black", size = 2, shape = 16) +
  labs(title = "Real GDP Forecast Conditional on M2",
       x = "Quarter",
       y = "RGDP (Log Scale)") +
  theme_minimal()


### Step 2: Plot series
library(forecast)
autoplot(cbind(inf, funds_rate), series = "l") +
  xlab("Time") +
  ylab("Value") +
  ggtitle("Multivariate Time Series Plot")


### determine the persistence of the model 
acf(rgdp, main="RGDP")

### find the optimal lag ###
growth_rates <- cbind(rgdp, funds_rate) %>% 
  na.omit()

VARselect(growth_rates, lag.max=10, type='const')

### 4 lags is optimal
growth_model <- VAR(growth_rates, p=3, type='const', season=NULL, exog=NULL)
plot(growth_model)
### model diagnostics ###
serial1 <- serial.test(growth_model, lags.pt=12)
stability <- stability(growth_model, type="OLS-CUSUM")

plot(stability)

### granger causality ###
### build 2 variable data sets ###
growth_causal <- causality(growth_model, cause = "funds_rate")

### impulse response function ###
growth_irf <- irf(growth_model, impulse="funds_rate", response="rgdp", n.ahead=8, boot=T)
plot(growth_irf, ylab="RGDP", main="Shock from Inflation")

early_data <- window(growth_rates, end=c(2020, 3))

VARselect(early_data, lag.max=10, type='const')

early_model <- VAR(early_data, p=6, type="const")

early_causality <- causality(early_model, cause="funds_rate")

early_preds <- predict(early_model, n.ahead = 13)
fanchart(early_preds, names="rgdp", main="RGDP Forecast")

### extract predicted values
predicted <- early_preds$fcst$rgdp[, "fcst"]

rgdp_preds <- early_preds$fcst$rgdp[, c("upper", "lower", "fcst")]

actual_rgdp <-  window(rgdp, start= c(2020, 3), end = c(2023,3))

rgdp_pred_forecast <- cbind(rgdp_preds, actual_rgdp)

### TIMKETK DATAFRAME TO USE DATES ###
rgdp_pred_forecast_df <- timetk::tk_tbl(rgdp_pred_forecast)

  ### RGDP FORECAST W INTEREST RATES ###
#### COMPARE W ACTUAL RGDP OVER THE SAME TIME PERIOD ###
funds_ratexrgdp <- ggplot(rgdp_pred_forecast_df, aes(x = index, y = rgdp_preds.fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = rgdp_preds.lower, ymax = rgdp_preds.upper), 
              fill = "blue", alpha = 0.2) +
  geom_point(aes(y = actual_rgdp), color = "black", size = 2, shape = 16) +
  labs(title = "RGDP Forecast Conditional on Funds Rate",
       x = "Quarter",
       y = "RGDP") +
  theme_minimal()


#####################################################################################
################# 
#################         RGDP x M2
#################
#####################################################################################

# Assuming 'm2_gdp' is your matrix time series
m2_gdp$log_rgdp <- log(m2_gdp[, "rgdp"])
m2_gdp$log_m2 <- log(m2_gdp[, "m2"])

###### logged
m2_gdp <- cbind(rgdp, m2) %>% 
  na.omit()     

### determine optimal lags ###
### optimal lags are 4
VARselect(m2_gdp)

### BUILD MODEL W 1 LAGS ###
m2_gdp_model <- VAR(m2_gdp, p=10, type="const")

### evaluate model ###
###
### SERIAL CORRELATION! FAILS PORTMANEAU TEST
###
serial.test(m2_gdp_model)
stability_infg <- stability(m2_gdp_model, type="OLS-CUSUM")

plot(stability_infg)

### GRANGER CAUSALITY & INSTANTANEOUS CAUSALITY
causality(m2_gdp_model, cause="m2")

### irf ###
### stradles 0 bc no granger causality ###
m2_gdp_irf <- irf(m2_gdp_model, impuse="m2", response="rgdp", n.ahead=8, boot=T)
plot(m2_gdp_irf)

### EXTRACT EARLY DATA ###
m2_gdp_early <- window(m2_gdp, end=c(2020, 3))

### build model again and this time PREDICT out ###
### build model 1
### find optimal lag
VARselect(m2_gdp_early)

### build model
early_m2_gdp_model <- VAR(m2_gdp_early, p=7, type="const")

### evaluate model
serial.test(early_m2_gdp_model)
stability(early_m2_gdp_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
### YES BOTH GRANGER & INSTANTANEOUS CAUSALITY
causality(early_m2_gdp_model, cause="m2")

### predict it
early_m2_gdp_pred <- predict(early_m2_gdp_model, n.ahead=13)

early_m2_gdp_forecast <- early_m2_gdp_pred$fcst$rgdp[, c("upper", "lower", "fcst")]

###compare to actual rgdp
rgdp_m2_forecast <- cbind(early_m2_gdp_forecast, actual_rgdp)

### TIMKETK DATAFRAME TO USE DATES ###
rgdp_m2_forecast_df <- timetk::tk_tbl(rgdp_m2_forecast)

### RGDP FORECAST W INTEREST RATES ###
#### COMPARE W ACTUAL RGDP OVER THE SAME TIME PERIOD ###
rgdp_m2_prior <- ggplot(rgdp_m2_forecast_df, aes(x = index, y = early_m2_gdp_forecast.fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = early_m2_gdp_forecast.lower, ymax = early_m2_gdp_forecast.upper), 
              fill = "blue", alpha = 0.2) +
  geom_point(aes(y = actual_rgdp), color = "black", size = 2, shape = 16) +
  labs(title = "RGDP Forecast Conditional on M2",
       x = "Quarter",
       y = "RGDP") +
  theme_minimal()

###
### future predictions for rgdp x m2
###

### predict it
m2_gdp_pred <- predict(m2_gdp_model, n.ahead=5)

m2_gdp_forecast <- m2_gdp_pred$fcst$rgdp[, c("upper", "lower", "fcst")]

### TIMKETK DATAFRAME TO USE DATES ###
rgdp_m2_forecast_future <- timetk::tk_tbl(m2_gdp_forecast)

# Assuming you have a time series of dates for the forecasted values
dates_forecast <- c("2023 Q4", "2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4")

dates_forecast_as_date <- as.Date(as.yearqtr(dates_forecast, format = "%Y Q%q"))
### RGDP FORECAST W INTEREST RATES ###
#### COMPARE W ACTUAL RGDP OVER THE SAME TIME PERIOD ###
rgdp_m2_future 
ggplot(rgdp_m2_forecast_future, aes(x = dates_forecast_as_date, y = fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill = "blue", alpha = 0.2) +
  labs(title = "RGDP Forecast Conditional on M2",
       x = "Quarter",
       y = "RGDP") +
  theme_minimal()

#####################################################################################
################# 
#################         RGDP x INFLATION
#################
#####################################################################################
inf_gdp <- cbind(rgdp, inf) %>% 
  na.omit()     

### determine optimal lags ###
### optimal lags are 4
VARselect(inf_gdp)

### BUILD MODEL W 1 LAGS ###
infgdp_model <- VAR(inf_gdp, p=9, type="const")

### evaluate model ###
serial.test(infgdp_model)
stability_infg <- stability(infgdp_model, type="OLS-CUSUM")

plot(stability_infg)

### determine granger causality
causality(infgdp_model, cause="inf")

### irf ###
### stradles 0 bc no granger causality ###
irf_infg <- irf(infgdp_model, impuse="inf", response="rgdp", n.ahead=8, boot=T)
plot(irf_infg)

### EXTRACT EARLY DATA ###
early_infg <- window(inf_gdp, end=c(2020, 3))

### build model again and this time PREDICT out ###
### build model 1
### find optimal lag
VARselect(early_infg)

### build model
early_infg_model <- VAR(early_infg, p=8, type="const")

### evaluate model
serial.test(early_infg_model)
stability(early_infg_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
### YES BOTH GRANGER & INSTANTANEOUS CAUSALITY
causality(early_infg_model, cause="inf")

### predict it
early_infg_pred <- predict(early_infg_model, n.ahead=13)

early_infg_forecast <- early_infg_pred$fcst$rgdp[, c("upper", "lower", "fcst")]

###compare to actual rgdp
rgdp_inf_forecast <- cbind(early_infg_forecast, actual_rgdp)

### TIMKETK DATAFRAME TO USE DATES ###
rgdp_inf_forecast_df <- timetk::tk_tbl(rgdp_inf_forecast)

### RGDP FORECAST W INTEREST RATES ###
#### COMPARE W ACTUAL RGDP OVER THE SAME TIME PERIOD ###
rgdp_inf_prior <- ggplot(rgdp_inf_forecast_df, aes(x = index, y = early_infg_forecast.fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = early_infg_forecast.lower, ymax = early_infg_forecast.upper), 
              fill = "blue", alpha = 0.2) +
  geom_point(aes(y = actual_rgdp), color = "black", size = 2, shape = 16) +
  labs(title = "RGDP Forecast Conditional on Inflation",
       x = "Quarter",
       y = "RGDP") +
  theme_minimal()

#####################################################################################
################# 
#################         PAYROLLS x INFLATION
#################
#####################################################################################
inf_jobs <- cbind(payroll, inf) %>% 
  na.omit()     

### determine optimal lags ###
### optimal lags are 4
VARselect(inf_jobs)

### BUILD MODEL W 4 LAGS ###
infjobs_model <- VAR(inf_jobs, p=5)

### evaluate model ###
serial.test(infjobs_model)
stability(infjobs_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
causality(infjobs_model, cause="inf")

### irf ###
### stradles 0 bc no granger causality ###
irf_infjobs <- irf(infjobs_model, impulse="inf", response="payroll", n.ahead=8, boot=T)
plot(irf_infjobs)

### EXTRACT EARLY DATA ###
early_infjob <- window(inf_jobs, end=c(2020, 3))

### build model again and this time PREDICT out ###
### build model 1
### find optimal lag
VARselect(early_infjob)

### build model
early_infjob_model <- VAR(early_infjob, p=10)

### evaluate model
serial.test(early_infjob_model)
stability(early_infjob_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
### YES BOTH GRANGER & INSTANTANEOUS CAUSALITY
causality(early_infjob_model, cause="inf")

### predict it
early_infjob_pred <- predict(early_infjob_model, n.ahead=13)

early_infjob_forecast <- early_infjob_pred$fcst$payroll[, c("upper", "lower", "fcst")]

###compare to actual payrolls
actual_payroll <-  window(payroll, start= c(2020, 3), end = c(2023,3))

payroll_inf_forecast <- cbind(early_infjob_forecast, actual_payroll)

### TIMKETK DATAFRAME TO USE DATES ###
payroll_inf_forecast_df <- timetk::tk_tbl(payroll_inf_forecast)

### PAYROLL FORECAST W INF ###
#### COMPARE W ACTUAL PAYROLL OVER THE SAME TIME PERIOD ###
payroll_inf_prior 
ggplot(payroll_inf_forecast_df, aes(x = index, y = early_infjob_forecast.fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = early_infjob_forecast.lower, ymax = early_infjob_forecast.upper), 
              fill = "blue", alpha = 0.2) +
  geom_point(aes(y = actual_payroll), color = "black", size = 2, shape = 16) +
  labs(title = "Payroll Forecast Conditional on Inflation (PCE)",
       x = "Quarter",
       y = "Payrolls") +
  theme_minimal()


#####################################################################################
################# 
#################         CONSUMPTION x INFLATION
#################
#####################################################################################
inf_cons <- cbind(consumption, inf) %>% 
  na.omit()  

### determine optimal lags ###
### optimal lags are 4
VARselect(inf_cons)

### BUILD MODEL W 4 LAGS ###
infcons_model <- VAR(inf_cons, p=10)

### evaluate model ###
serial.test(infcons_model)
stability(infcons_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
causality(infcons_model, cause="inf")

### irf ###
### stradles 0 bc no granger causality ###
irf_infcons <- irf(infcons_model, impulse="inf", response="consumption", n.ahead=8, boot=T)
plot(irf_infcons)

### EXTRACT EARLY DATA ###
early_infcons <- window(inf_cons, end=c(2020, 3))

### build model again and this time PREDICT out ###
### build model 1
### find optimal lag
VARselect(early_infcons)

### build model
early_infcons_model <- VAR(early_infcons, p=9)

### evaluate model
serial.test(early_infcons_model)
stability(early_infcons_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
### YES BOTH GRANGER & INSTANTANEOUS CAUSALITY
causality(early_infcons_model, cause="inf")

### predict it
early_infcons_pred <- predict(early_infcons_model, n.ahead=13)

early_infcons_forecast <- early_infcons_pred$fcst$consumption[, c("upper", "lower", "fcst")]

###compare to actual payrolls
actual_consumption <-  window(consumption, start= c(2020, 3), end = c(2023,3))

consumption_inf_forecast <- cbind(early_infcons_forecast, actual_consumption)

### TIMKETK DATAFRAME TO USE DATES ###
consumption_inf_forecast_df <- timetk::tk_tbl(consumption_inf_forecast)
colnames(consumption_inf_forecast_df)

### Consumption FORECAST W INF ###
#### COMPARE W ACTUAL PAYROLL OVER THE SAME TIME PERIOD ###
consumption_inf_prior <- ggplot(consumption_inf_forecast_df, aes(x = index, y = early_infcons_forecast.fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = early_infcons_forecast.lower, ymax = early_infcons_forecast.upper), 
              fill = "blue", alpha = 0.2) +
  geom_point(aes(y = actual_consumption), color = "black", size = 2, shape = 16) +
  labs(title = "Consumption Forecast Conditional on Inflation",
       x = "Quarter",
       y = "Consumption") +
  theme_minimal()

#####################################################################################
################# 
#################         CONSUMPTION x FUNDS RATE
#################
#####################################################################################
funds_cons <- cbind(consumption, funds_rate) %>% 
  na.omit()  

### determine optimal lags ###
### optimal lags are 4
VARselect(funds_cons)

### BUILD MODEL W 4 LAGS ###
fundscons_model <- VAR(funds_cons, p=2)

### evaluate model ###
serial.test(fundscons_model)
stability(fundscons_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
causality(fundscons_model, cause="funds_rate")

### irf ###
### stradles 0 bc no granger causality ###
irf_fundscons <- irf(fundscons_model, impulse="funds_rate", response="consumption", n.ahead=8, boot=T)
plot(irf_fundscons)

### EXTRACT EARLY DATA ###
early_fundscons <- window(funds_cons, end=c(2020, 3))

### build model again and this time PREDICT out ###
### build model 1
### find optimal lag
VARselect(early_fundscons)

### build model
early_fundscons_model <- VAR(early_fundscons, p=6)

### evaluate model
serial.test(early_fundscons_model)
stability(early_fundscons_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
### YES BOTH GRANGER & INSTANTANEOUS CAUSALITY
causality(early_fundscons_model, cause="funds_rate")

### predict it
early_fundscons_pred <- predict(early_fundscons_model, n.ahead=13)

early_fundscons_forecast <- early_fundscons_pred$fcst$consumption[, c("upper", "lower", "fcst")]

###compare to actual payrolls
actual_consumption <-  window(consumption, start= c(2020, 3), end = c(2023,3))

consumption_funds_forecast <- cbind(early_fundscons_forecast, actual_consumption)

### TIMKETK DATAFRAME TO USE DATES ###
consumption_funds_forecast_df <- timetk::tk_tbl(consumption_funds_forecast)

### Consumption FORECAST W INF ###
#### COMPARE W ACTUAL PAYROLL OVER THE SAME TIME PERIOD ###
consumption_funds_prior <- ggplot(consumption_funds_forecast_df, aes(x = index, y = early_fundscons_forecast.fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = early_fundscons_forecast.lower, ymax = early_fundscons_forecast.upper), 
              fill = "blue", alpha = 0.2) +
  geom_point(aes(y = actual_consumption), color = "black", size = 2, shape = 16) +
  labs(title = "Consumption Forecast Conditional on Funds Rate",
       x = "Quarter",
       y = "Consumption") +
  theme_minimal()

#####################################################################################
################# 
#################         UNEMPLOYMENT x FUNDS RATE
#################
#####################################################################################
          
funds_unemploy <- cbind(funds_rate, unemploy) %>% 
  na.omit()  

### determine optimal lags ###
### optimal lags are 4
VARselect(funds_unemploy)

### BUILD MODEL W 4 LAGS ###
fundsunemploy_model <- VAR(funds_unemploy, p=3)

### evaluate model ###
serial.test(fundsunemploy_model)
stability(fundsunemploy_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
causality(fundsunemploy_model, cause="funds_rate")

### irf ###
### stradles 0 bc no granger causality ###
irf_fundsunemploy <- irf(fundsunemploy_model, impulse="funds_rate", response="unemploy", n.ahead=8, boot=T)
plot(irf_fundsunemploy)

### EXTRACT EARLY DATA ###
early_fundsunemploy <- window(funds_unemploy, end=c(2020, 3))

### build model again and this time PREDICT out ###
### build model 1
### find optimal lag
VARselect(early_fundsunemploy)

### build model
early_fundsunemploy_model <- VAR(early_fundsunemploy, p=3)

### evaluate model
serial.test(early_fundsunemploy_model)
stability(early_fundsunemploy_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
### YES BOTH GRANGER & INSTANTANEOUS CAUSALITY
causality(early_fundsunemploy_model, cause="funds_rate")

### predict it
early_fundsunemploy_pred <- predict(early_fundsunemploy_model, n.ahead=13)

early_fundsunemploy_forecast <- early_fundsunemploy_pred$fcst$unemploy[, c("upper", "lower", "fcst")]

###compare to actual payrolls
actual_unemploy <-  window(unemploy, start= c(2020, 3), end = c(2023,3))

unemploy_funds_forecast <- cbind(early_fundsunemploy_forecast, actual_unemploy)

### TIMKETK DATAFRAME TO USE DATES ###
unemploy_funds_forecast_df <- timetk::tk_tbl(unemploy_funds_forecast)

### Consumption FORECAST W INF ###
#### COMPARE W ACTUAL PAYROLL OVER THE SAME TIME PERIOD ###
unemploy_funds_prior <- ggplot(unemploy_funds_forecast_df, aes(x = index, y = early_fundsunemploy_forecast.fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = early_fundsunemploy_forecast.lower, ymax = early_fundsunemploy_forecast.upper), 
              fill = "blue", alpha = 0.2) +
  geom_point(aes(y = actual_unemploy), color = "black", size = 2, shape = 16) +
  labs(title = "Unemployment Forecast Conditional on Funds Rate",
       x = "Quarter",
       y = "Unemployment") +
  theme_minimal()


#####################################################################################
################# 
#################         UNEMPLOYMENT x INFLATION
#################
#####################################################################################

inf_unemploy <- cbind(inf, unemploy) %>% 
  na.omit()  

### determine optimal lags ###
### optimal lags are 4
VARselect(inf_unemploy)

### BUILD MODEL W 4 LAGS ###
infunemploy_model <- VAR(inf_unemploy, p=9)

### evaluate model ###
serial.test(infunemploy_model)
stability(infunemploy_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
causality(infunemploy_model, cause="inf")

### irf ###
### stradles 0 bc no granger causality ###
irf_infunemploy <- irf(infunemploy_model, impulse="inf", response="unemploy", n.ahead=8, boot=T)
plot(irf_infunemploy)

### EXTRACT EARLY DATA ###
early_infunemploy <- window(inf_unemploy, end=c(2020, 3))

### build model again and this time PREDICT out ###
### build model 1
### find optimal lag
VARselect(early_infunemploy)

### build model
early_infunemploy_model <- VAR(early_infunemploy, p=9)

### evaluate model
serial.test(early_infunemploy_model)
stability(early_fundsunemploy_model, type="OLS-CUSUM") %>% plot()

### determine granger causality
### YES BOTH GRANGER & INSTANTANEOUS CAUSALITY
causality(early_infunemploy_model, cause="inf")

### predict it
early_infunemploy_pred <- predict(early_infunemploy_model, n.ahead=13)

early_infunemploy_forecast <- early_infunemploy_pred$fcst$unemploy[, c("upper", "lower", "fcst")]

###compare to actual payrolls
actual_unemploy <-  window(unemploy, start= c(2020, 3), end = c(2023,3))

unemploy_inf_forecast <- cbind(early_infunemploy_forecast, actual_unemploy)

### TIMKETK DATAFRAME TO USE DATES ###
unemploy_inf_forecast_df <- timetk::tk_tbl(unemploy_inf_forecast)

### Consumption FORECAST W INF ###
#### COMPARE W ACTUAL PAYROLL OVER THE SAME TIME PERIOD ###
unemploy_inf_prior <- ggplot(unemploy_inf_forecast_df, aes(x = index, y = early_infunemploy_forecast.fcst)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = early_infunemploy_forecast.lower, ymax = early_infunemploy_forecast.upper), 
              fill = "blue", alpha = 0.2) +
  geom_point(aes(y = actual_unemploy), color = "black", size = 2, shape = 16) +
  labs(title = "Unemployment Forecast Conditional on Inflation",
       x = "Quarter",
       y = "Unemployment") +
  theme_minimal()


library(cowplot)
plot_grid(funds_ratexrgdp, rgdp_inf_prior, consumption_funds_prior, consumption_inf_prior,
          unemploy_funds_prior, unemploy_inf_prior,
          ncol = 2)


#####################################################################################
#####################################################################################
################# 
#################         FORECASTS FOR 2024
#################
#####################################################################################
#####################################################################################

##### 
#### RGDP ~ FUNDS RATE
####
VARselect(growth_rates, lag.max=10, type='const')
head(growth_rates)

### 4 lags is optimal
growth_model <- VAR(growth_rates, p=3, type='const', season=NULL, exog=NULL)
plot(growth_model)
### model diagnostics ###
serial1 <- serial.test(growth_model, lags.pt=12)
stability <- stability(growth_model, type="OLS-CUSUM")

plot(stability)

### granger causality ###
### build 2 variable data sets ###
growth_causal <- causality(growth_model, cause = "funds_rate")

### impulse response function ###
growth_irf <- irf(growth_model, impulse="funds_rate", response="rgdp", n.ahead=8, boot=T)
plot(growth_irf, ylab="RGDP", main="Shock from Funds Rate")

future_preds <- predict(growth_model, n.ahead = 4)

### extract predicted values
forecast_rgdp <- future_preds$fcst$rgdp[, "fcst"]

rgdp_fore <- future_preds$fcst$rgdp[, c("upper", "lower", "fcst")]

### TIMKETK DATAFRAME TO USE DATES ###
rgdp_forecast_df <- timetk::tk_tbl(rgdp_fore)

rgdp_forecast_df

# Assuming you have a time series of dates for the forecasted values
dates_forecast <- c("2023 Q4", "2024 Q1", "2024 Q2", "2024 Q3")

dates_forecast_as_date <- as.Date(as.yearqtr(dates_forecast, format = "%Y Q%q"))


# Plotting the forecasted values with ribbons
rgdp_forecast <- ggplot() +
  geom_line(aes(x = dates_forecast_as_date, y = rgdp_forecast_df$fcst, color = "blue", size = 1)) +
  geom_ribbon(aes(x = dates_forecast_as_date, ymin = rgdp_forecast_df$lower, ymax = rgdp_forecast_df$upper),
              fill = "blue", alpha = 0.2) +
  labs(title = "RGDP Forecast into 2024",
       x = "Date",
       y = "RGDP") +
  theme_minimal() +
  theme(legend.position = "none") 

##### 
#### RGDP ~ INTEREST RATES
####
VARselect(growth_rates, lag.max=10, type='const')
head(growth_rates)

### 4 lags is optimal
growth_model <- VAR(growth_rates, p=3, type='const', season=NULL, exog=NULL)
plot(growth_model)
### model diagnostics ###
serial1 <- serial.test(growth_model, lags.pt=12)
stability <- stability(growth_model, type="OLS-CUSUM")

plot(stability)

### granger causality ###
### build 2 variable data sets ###
growth_causal <- causality(growth_model, cause = "funds_rate")

### impulse response function ###
growth_irf <- irf(growth_model, impulse="funds_rate", response="rgdp", n.ahead=8, boot=T)
plot(growth_irf, ylab="RGDP", main="Shock from Funds Rate")

future_preds <- predict(growth_model, n.ahead = 4)

### extract predicted values
forecast_rgdp <- future_preds$fcst$rgdp[, "fcst"]

rgdp_fore <- future_preds$fcst$rgdp[, c("upper", "lower", "fcst")]

### TIMKETK DATAFRAME TO USE DATES ###
rgdp_forecast_df <- timetk::tk_tbl(rgdp_fore)

rgdp_forecast_df

# Assuming you have a time series of dates for the forecasted values
dates_forecast <- c("2023 Q4", "2024 Q1", "2024 Q2", "2024 Q3")

dates_forecast_as_date <- as.Date(as.yearqtr(dates_forecast, format = "%Y Q%q"))


# Plotting the forecasted values with ribbons
rgdp_forecast <- ggplot() +
  geom_line(aes(x = dates_forecast_as_date, y = rgdp_forecast_df$fcst, color = "blue", size = 1)) +
  geom_ribbon(aes(x = dates_forecast_as_date, ymin = rgdp_forecast_df$lower, ymax = rgdp_forecast_df$upper),
              fill = "blue", alpha = 0.2) +
  labs(title = "RGDP Forecast into 2024",
       x = "Date",
       y = "RGDP") +
  theme_minimal() +
  theme(legend.position = "none") 

######
##### RGDP ~ INTEREST RATES
####
VARselect(inf_gdp, lag.max=10, type='const')

### 4 lags is optimal
growth_model <- VAR(inf_gdp, p=9, type='const', season=NULL, exog=NULL)
plot(growth_model)
### model diagnostics ###
serial1 <- serial.test(growth_model, lags.pt=12)
stability <- stability(growth_model, type="OLS-CUSUM")

plot(stability)

### granger causality ###
### build 2 variable data sets ###
growth_causal <- causality(growth_model, cause = "inf")

### impulse response function ###
growth_irf <- irf(growth_model, impulse="inf", response="rgdp", n.ahead=5, boot=T)
plot(growth_irf, ylab="RGDP", main="Shock from Inflation")

future_preds <- predict(growth_model, n.ahead = 5)

### extract predicted values
forecast_rgdp <- future_preds$fcst$rgdp[, "fcst"]

rgdp_fore <- future_preds$fcst$rgdp[, c("upper", "lower", "fcst")]
head(rgdp_fore)
### TIMKETK DATAFRAME TO USE DATES ###
rgdp_forecast_df <- timetk::tk_tbl(rgdp_fore)

rgdp_forecast_df

# Assuming you have a time series of dates for the forecasted values
dates_forecast <- c("2023 Q4", "2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4")

dates_forecast_as_date <- as.Date(as.yearqtr(dates_forecast, format = "%Y Q%q"))


# Plotting the forecasted values with ribbons
rgdp_forecast <- ggplot() +
  geom_line(aes(x = dates_forecast_as_date, y = rgdp_forecast_df$fcst, color = "blue", size = 1)) +
  geom_ribbon(aes(x = dates_forecast_as_date, ymin = rgdp_forecast_df$lower, ymax = rgdp_forecast_df$upper),
              fill = "blue", alpha = 0.2) +
  labs(title = "RGDP Forecast into 2024",
       x = "Date",
       y = "RGDP") +
  theme_minimal() +
  theme(legend.position = "none") 
#### future_preds <- predict(growth_model, n.ahead = 4)

###
### RGDP ~ M2
###

### extract predicted values
forecast_rgdp <- future_preds$fcst$rgdp[, "fcst"]

rgdp_fore <- future_preds$fcst$rgdp[, c("upper", "lower", "fcst")]

### TIMKETK DATAFRAME TO USE DATES ###
rgdp_forecast_df <- timetk::tk_tbl(rgdp_fore)

rgdp_forecast_df

# Assuming you have a time series of dates for the forecasted values
dates_forecast <- c("2023 Q4", "2024 Q1", "2024 Q2", "2024 Q3")

dates_forecast_as_date <- as.Date(as.yearqtr(dates_forecast, format = "%Y Q%q"))


# Plotting the forecasted values with ribbons
rgdp_forecast <- ggplot() +
  geom_line(aes(x = dates_forecast_as_date, y = rgdp_forecast_df$fcst, color = "blue", size = 1)) +
  geom_ribbon(aes(x = dates_forecast_as_date, ymin = rgdp_forecast_df$lower, ymax = rgdp_forecast_df$upper),
              fill = "blue", alpha = 0.2) +
  labs(title = "RGDP Forecast into 2024",
       x = "Date",
       y = "RGDP") +
  theme_minimal() +
  theme(legend.position = "none") 


#### plot final 2024 forecasts
plotgrid(rgdp_forecast)


