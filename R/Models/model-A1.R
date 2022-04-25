# pacakges 
library(qqplotr)
library(forecast)
library(tidyverse)
library(gridExtra)
source("R/utils/function-2.R")

# get data ----
kc <- read.csv("data/kc.csv")
so2 <- monthly(kc,"so2") %>%
  ungroup() %>% 
  select(so2) %>% 
  sapply(as.numeric) %>% 
  c() %>% 
  log() %>% # log transformation  
  diff() # 1st order differencing

n <- length(so2)

# basic EDA ----
g <- ggplot(data.frame(month=1:n,so2=so2),
             aes(x=month,y=so2)) + 
  geom_line() + 
  labs(y="ln(so2) *d=1")
plot(g)

# ARMA(p,q) test ----
ma <- ggAcf(so2) +
  ggtitle("ACF: SO2") + 
  theme(plot.title=element_text(hjust=0.5))

ar <- ggAcf(so2,type="partial") +
  ggtitle("PACF: SO2") + 
  theme(plot.title=element_text(hjust=0.5))

grid.arrange(ma,ar)

# ARMA model ----
fit.arma <- auto.arima(so2,ic="aic") # ARMA(1,1)

# error analysis ----
rPlot(fit.arma)

filename <- "output/model/model3A_ARMA.png"
png(filename)
dplots(fit.arma)
dev.off()
