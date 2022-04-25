# packages 
library(tidyverse)
library(gridExtra)
source("R/Functions/function-2.R")

# get data ----
kc <- read.csv("data/kc.csv")

so2 <- monthly(kc,"so2") %>%
  ungroup() %>% 
  select(so2,Date)

o3 <- monthly(kc,"o3") %>%
  ungroup() %>% 
  select(o3,Date) 

# figure 1 ----
f1 <- ggplot(so2,aes(x=Date,y=so2)) + geom_line() + labs(x="time")
f2 <- ggplot(o3,aes(x=Date,y=o3)) + geom_line() + labs(x="time")

filename <- "output/figure/fig2-2.png"
png(filename)
grid.arrange(f1,f2)
dev.off()

