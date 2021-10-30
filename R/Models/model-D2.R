# packages 
library(rstan)
library(extRemes)
library(tidyverse)
library(gridExtra)
source("R/Functions/function-2.R")

# get data ----
kc <- read.csv("data/kc.csv")

o3 <- kc %>%
  drop_na(o3) %>%
  select(o3) %>% 
  sapply(as.numeric) %>% # convert to matrix
  c()

n <- length(o3)

# state space model: GP ----
sample_iterations <- 1e4
data <-list(ymin=u_so2,
            N=ny_so2,
            y=y_so2) # exceedanecs 

mod.gpd <- stan(file='stan/model-D2.stan',
                data=data,
                seed=100,
                chains=3,
                iter=sample_iterations,
                warmup=sample_iterations/5)

ms <- rstan::extract(mod.gpd)
mcmc <- data.frame(scale=ms$sigma,
                   shape=ms$k)
