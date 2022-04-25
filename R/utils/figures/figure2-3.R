# packages 
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

n1 <- length(so2)
n2 <- length(o3)

# figure 1 ----
f1 <- ggplot(data.frame(month=1:n1,so2=so2),
             aes(x=month,y=so2)) + 
  geom_line()
f2 <- ggplot(data.frame(month=1:n2,o3=o3),
             aes(x=month,y=o3)) + 
  geom_line()

filename <- "output/figure/fig2-3.png"
png(filename)
grid.arrange(f1,f2)
dev.off()
