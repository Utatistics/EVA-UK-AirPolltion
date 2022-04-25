# packages
library(tidyverse)
library(gridExtra)
source("R/Functions/function-2.R")

# get data ----
kc <- read.csv("data/kc.csv")

# figure 3 ----
f1 <- ggplot(data=monthly(kc,"so2"),
            aes_string(x="ws",y="so2")) + 
  geom_point(shape=21,color="black",fill="skyblue") + 
  labs(x="windspeed")

f2 <- ggplot(data=monthly(kc,"so2"),
             aes_string(x="air_temp",y="so2")) + 
  geom_point(shape=21,color="black",fill="skyblue") + 
  labs(x="air temperature")

f3 <- ggplot(data=monthly(kc,"o3"),
             aes_string(x="ws",y="o3")) +
  geom_point(shape=21,color="black",fill="green") +
  labs(x="windspeed")


f4 <- ggplot(data=monthly(kc,"o3"),
             aes_string(x="air_temp",y="o3")) +
  geom_point(shape=21,color="black",fill="green") +
  labs(x="air temperature")

filename <- "output/figure/fig2-6.png"
png(filename)
grid.arrange(f1,f2,f3,f4)
dev.off()
