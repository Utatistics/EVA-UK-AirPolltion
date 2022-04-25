# packages 
library(rstan)
library(tidyverse)

# simulating from a Gaussian process
n = 100
sample_iterations <- 5e3
data <-list(y=rnorm(n),N=n) 

mod <- stan(file='repository/stan/model-D4.stan',
            data=data,
            seed=1,
            chains=4,
            iter=sample_iterations,
            warmup=sample_iterations/5)

ms <- rstan::extract(mod)

mcmc <- data.frame(mu=ms$mu,
                   xi=ms$xi,
                   mu_0=ms$mu_0,
                   gamma=ms$gamma,
                   beta1=ms$beta1,
                   beta2=ms$beta2,
                   sigma=ms$sigma,
                   yrep=ms$yrep)

stan_trace(mod,
           pars=c("xi","gamma","beta1","beta2"),
           inc_warmup=TRUE) + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

save(mod,file="data/RData/mcmc-gp.RData")

