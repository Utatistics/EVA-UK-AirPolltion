# packages 
library(rugarch)
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
  diff()  # 1st order differencing

n <- length(so2)

# basic EDA ----
g <- ggplot(data.frame(time=1:n,so2=so2),
            aes(x=time,y=so2)) + 
  geom_line() + 
  labs(y="so2 *d=1")

plot(g)

# ARMA-GARCH model ----
spec1 <- ugarchspec(
  variance.model=list(model="sGARCH",garchOrder=c(1,1)),
  mean.model=list(armaOrder=c(0,0),include.mean=TRUE),
  distribution.model="norm")

spec2 <- ugarchspec(
  variance.model=list(model="sGARCH",garchOrder=c(1,1)),
  mean.model=list(armaOrder=c(1,1),include.mean=TRUE),
  distribution.model="norm")

mod1 <- ugarchfit(spec=spec1,data=so2,solver="hybrid")
mod2 <- ugarchfit(spec=spec2,data=so2,solver="hybrid")

# model diagnostics ----
dplots(mod1)
dplots(mod2)

filename <- "output/model/model3B_GARCH.png"
png(filename)
dplots(mod1)
dev.off()

filename <- "output/model/model3C_ARMA-GARCH.png"
png(filename)
dplots(mod2)
dev.off()
