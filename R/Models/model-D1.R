# packages 
library(rstan)
library(extRemes)
library(tidyverse)
library(gridExtra)
source("R/Functions/function-2.R")

# get data ----
kc <- read.csv("data/kc.csv")
so2 <- kc %>%
  drop_na(so2) %>%
  select(so2) %>% 
  sapply(as.numeric) %>% # convert to matrix
  c()

n <- length(so2)

# mrl plot ----
mrlPlot(so2)

u <- 100 # threshold determined 
y <- so2[so2 > u]
n_y <- length(y)

# extRemes *GP model ====
fit.gp <- fevd(x=y,
            threshold=u,
            type="GP")

# stan *bayesian modeling ====
sample_iterations <- 5e3
data <-list(ymin=u,
            N=n_y,
            y=y) # exceedanecs 

mod.gp <- stan(file='stan/model-D1.stan',
                data=data,
                seed=100,
                chains=4,
                iter=sample_iterations,
                warmup=sample_iterations/5)

ms <- rstan::extract(mod.gp)
mcmc <- data.frame(scale=ms$sigma,
                   shape=ms$k)

# CI compasison ----
CI <- rbind(
  ci.scale <- ci.fevd(fit.gp,type="parameter",which.par=1)[1:3] %>% unname(),
  ci.scale.b <- quantile(mcmc$scale,c(.025,.5,.975)) %>% unname(),
  ci.shape <- ci.fevd(fit.gp,type="parameter",which.par=2)[1:3] %>% unname(),
  ci.shape.b <- quantile(mcmc$shape,c(.025,.5,.975)) %>% unname()
)
colnames(CI) <- c("lwr","mid","upr")
rownames(CI) <- c("sigma","sigma.b","xi","xi.b")

CI <- as.data.frame(CI) %>%
  mutate(length = upr-lwr)

CI