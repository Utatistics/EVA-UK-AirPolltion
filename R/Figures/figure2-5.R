# pacakges 
library(qqplotr)
library(forecast)
library(tidyverse)
library(gridExtra)
source("R/Functions/function-2.R")

# get data ----
kc <- read.csv("data/kc.csv")

so2 <- monthly(kc,"so2") %>%
  ungroup() %>% 
  select(so2) %>% 
  sapply(as.numeric) %>% 
  c() %>% 
  log() %>% # log transformation  
  diff() # 1st order differencing

o3 <- monthly(kc,"o3") %>%
  ungroup() %>% 
  select(o3) %>% 
  sapply(as.numeric) %>% 
  c() %>% 
  diff(lag=12) 

# ARMA model ----
fit1 <- auto.arima(so2,ic="aic") # ARMA(1,1)
fit2<- auto.arima(o3,ic="aic") # ARMA(1,1)

# error analysis ----
filename <- "output/figure/fig2-5-a.png"
png(filename)
rPlot(fit1)
dev.off()

filename <- "output/figure/fig2-5-b.png"
png(filename)
rPlot(fit2)
dev.off()
