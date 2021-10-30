# packages 
library(rstan)
library(extRemes)
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
  log() %>% # log transformation  
  diff() # 1st order differencing

n <- length(so2)

# extRemes *block maxima model ----
fit.gev <- fevd(so2,type="GEV")
params <- distill(fit.gev) # extracting parameters 
s.e <- parcov.fevd(fit.gev) %>% diag() %>% sqrt()

qq_loc <- qnorm(p=c(.025,.975),
            mean=params["location"],
            sd=s.e["location"])
qq_scale <- qnorm(p=c(.025,.975),
                mean=params["scale"],
                sd=s.e["scale"])
qq_shape <- qnorm(p=c(.025,.975),
                mean=params["shape"],
                sd=s.e["shape"])

# stan *bayesian modeling ----
sample_iterations <- 5e3
data <-list(y=so2,N=n) # monthly maxima

mod.gev <- stan(file='stan/model-D3.stan',
                data=data,
                seed=1,
                chains=4,
                iter=sample_iterations,
                warmup=sample_iterations/5)

# storing the samplings ----
ms <- rstan::extract(mod.gev)
mcmc <- data.frame(loc=ms$mu,
                   scale=ms$sigma,
                   shape=ms$xi,
                   yrep=ms$yrep)

save(mod.gev,ms,mcmc,file="data/mcmc-model-D3.RData")

# diagnostic plots ----
stan_dens(mod.gev,pars=c("mu","sigma","xi"))
stan_trace(mod.gev,pars=c("mu","sigma","xi"),inc_warmup=TRUE) + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

# CI compasison ----
CI <- rbind(
  ci.fevd(fit.gev,type="parameter",which.par=1)[1:3] %>% unname(),
  quantile(mcmc$loc,c(.025,.5,.975)) %>% unname(),
  ci.fevd(fit.gev,type="parameter",which.par=2)[1:3] %>% unname(),
  quantile(mcmc$scale,c(.025,.5,.975)) %>% unname(),
  ci.fevd(fit.gev,type="parameter",which.par=3)[1:3] %>% unname(),
  quantile(mcmc$shape,c(.025,.5,.975)) %>% unname()
)
colnames(CI) <- c("lwr","mid","upr")
rownames(CI) <- c("mu","mu.b","sigma","sigma.b","xi","xi.b")

CI <- as.data.frame(CI) %>%
  mutate(length = upr-lwr)

CI

# density comparison ----
load("data/mcmc-model-D3.RData")

g_loc <- ggplot(mcmc,aes(x=loc)) +
  geom_density(color="black",fill="blue",alpha=.65) + 
  stat_function(data=data.frame(z=seq(qq_loc[1],qq_loc[2],len=100)),
                aes(x=z),
                fun=dnorm,
                args=list(mean=params["location"],sd=s.e["location"]),
                linetype="dashed",color="yellow",
                size=.75) + 
  labs(y="density")

g_scale <- ggplot(mcmc,aes(x=scale)) +
  geom_density(color="black",fill="blue",alpha=.65) + 
  stat_function(data=data.frame(z=seq(qq_scale[1],qq_scale[2],len=100)),
                aes(x=z),
                fun=dnorm,
                args=list(mean=params["scale"],sd=s.e["scale"]),
                linetype="dashed",color="yellow",
                size=.75) +
  labs(y="density")

g_shape <- ggplot(mcmc,aes(x=shape)) +
  geom_density(color="black",fill="blue",alpha=.65) + 
  stat_function(data=data.frame(z=seq(qq_shape[1],qq_shape[2],len=100)),
                aes(x=z),
                fun=dnorm,
                args=list(mean=params["shape"],sd=s.e["shape"]),
                linetype="dashed",color="yellow",
                size=.75) +
  labs(y="density")

filename <- "output/figure/fig-5.2.png"
png(filename)
grid.arrange(g_loc,g_scale,g_shape,ncol=3)
dev.off()
