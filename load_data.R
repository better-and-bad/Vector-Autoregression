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
fredr_set_key("YOUR-KEY")


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
### INCLUDE COMMODITY PRICES ?? ###