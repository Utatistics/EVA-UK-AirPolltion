# packages 
library(extRemes)
library(tidyverse)
library(gridExtra)
source("R/utils/function-2.R")
source("R/utils/function-4.R")

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

# block maxima model ----
fit.gev <- fevd(so2,type="GEV")
params <- distill(fit.gev) # extracting parameters 

# diagnostic plots ----
filename <- "output/model/model-B.png"
png(filename)
dPlotGEV(fit.gev)
dev.off()

# parameter CI ----
CI <- data.frame(loc=ci.fevd(fit.gev,type="parameter",which.par=1) %>% c(),
                 scale=ci.fevd(fit.gev,type="parameter",which.par=2) %>% c(),
                 shape=ci.fevd(fit.gev,type="parameter",which.par=3) %>% c()) %>% t()

H <- fit.gev$results[["hessian"]]

s.e <- parcov.fevd(fit.gev) %>% diag() %>% sqrt()

