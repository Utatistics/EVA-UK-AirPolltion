# packages
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
  log() %>% 
  diff()

o3 <- monthly(kc,"o3") %>%
  ungroup() %>% 
  select(o3) %>% 
  sapply(as.numeric) %>% 
  c() %>% 
  diff(lag=12) 

# figure 4 ----
f1 <- ggAcf(so2) + ggtitle("")
f2 <- ggAcf(o3) + ggtitle("")

filename <- "output/figure/fig2-4.png"
png(filename)
grid.arrange(f1,f2)
dev.off()
