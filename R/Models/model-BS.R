# packages 
library(extRemes)
library(tidyverse)
library(gridExtra)
source("R/utils/function-2.R")
source("R/utils/function-4.R")

# get data ----
kc <- read.csv("data/kc.csv")

o3 <- monthly(kc,"o3") %>%
  ungroup() %>% 
  select(o3) %>% 
  sapply(as.numeric) %>% 
  c() %>% 
  diff(lags=12) %>%  # differencing with lag=12
  diff(difference=2)

n <- length(o3)

# basic EDA ----
g <- ggplot(data.frame(month=1:n_o3,o3=o3),
               aes(x=month,y=o3)) + 
  geom_line() + 
  labs(y="o3 *d=12")

plot(g)

# block maxima model ----
fit.gev <- fevd(o3*.01,type="GEV") # numerical error: log(z) NaN produced.
params <- distill(fit.gev) 

# diagnostic plots ----
filename <- "output/model/model-BS.png"
png(filename)
dPlotGEV(fit.gev)
dev.off()

# parameter CI ----
CI <- data.frame(loc=ci.fevd(fit.gev,type="parameter",which.par=1) %>% c(),
                 scale=ci.fevd(fit.gev,type="parameter",which.par=2) %>% c(),
                 shape=ci.fevd(fit.gev,type="parameter",which.par=3) %>% c()) %>% t()

H <- fit.gev$results[["hessian"]]

s.e <- parcov.fevd(fit.gev) %>% diag() %>% sqrt()

