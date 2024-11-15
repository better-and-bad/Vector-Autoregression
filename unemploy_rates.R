

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