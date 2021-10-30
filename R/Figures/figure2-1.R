# packages
library(tidyverse)
library(gridExtra)
source("R/Functions/function-2.R")

# get data  ----
vars <- c("co","nox","no2","no","o3","so2","pm10","pm2.5") # names of pollution 
kc <- read.csv("data/kc.csv") # site = "London N. "Kensington"

# plot function ---- 
f <- function(var){
  d <- monthly(kc,var) %>%
    drop_na(any_of(var))

  g <- ggplot(d,aes_string(x="Date",y=var)) +
    geom_line() + 
    ggtitle(var) + 
    labs(x="",y="") + 
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          plot.title=element_text(hjust=.5)) 
  return(g)
}

# plotting ----
g_co <- f("co")
g_nox <- f("nox")
g_no2 <- f("no2")
g_no <- f("no")
g_o3 <- f("o3")
g_so2 <- f("so2")
g_pm10 <- f("pm10")
g_pm2.5 <- f("pm2.5")

# saving the plot ----
filename <- paste0("output/figure/fig2-1.png")
png(filename)
grid.arrange(g_no,g_nox,g_no2, 
             g_co,g_o3,g_so2, 
             g_pm10,g_pm2.5)
dev.off()
