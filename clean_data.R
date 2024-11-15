


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