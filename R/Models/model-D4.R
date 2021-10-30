# packages 
library(rstan)
library(tidyverse)
library(gridExtra)
source("R/Functions/function-2.R")
source("R/Functions/function-7.R")

# get data ----
kc <- read.csv("data/kc.csv")

so2 <- monthly(kc,"so2") %>%
  ungroup() %>% 
  select(so2) %>% 
  sapply(as.numeric) %>% 
  c() 

n <- length(so2)

# state space model: GEV ----
sample_iterations <- 5e3
data <-list(y=so2,N=n_so2) 

mod.gev <- stan(file='stan/model-D4.stan',
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

mcmc_summary <- summary(mod.gev)[["summary"]] %>% as.data.frame() 

save(mod.gev,ms,mcmc,mcmc_summary,file="data/mcmc-model-D4.RData")
load("data/mcmc-model-D4.RData")

# diagnostic plots ----
stan_trace(mod.gev,pars=c("xi","gamma","beta1","beta2"),inc_warmup=TRUE) + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

# credible interval ----
ci <- apply(X=mcmc, 
            MARGIN=2, # apply to columns 
            FUN=quantile,
            probs=c(.025,.5,.975)) %>% # augument of FUN: # 95% CI
  t() %>% 
  as.data.frame() 

colnames(ci) <- c("lwr","mid","upr")

ci_loc <- ci[1:n_so2,]
ci_scale <- ci[(n_so2+1):(n_so2*2),]
ci_shape <- ci[n_so2+2,]
ci_pred <- tail(ci,n=n_so2)
ci_state <- data.frame(lwr=ci_loc$lwr + ci_scale$lwr*(gamma(1-ci_shape$lwr))/ci_shape$lwr,
                       mid=ci_loc$mid + ci_scale$mid*(gamma(1-ci_shape$mid))/ci_shape$mid,
                       upr=ci_loc$upr + ci_scale$upr*(gamma(1-ci_shape$upr))/ci_shape$upr,
                       lwr.p=ci_pred$lwr,
                       mid.p=ci_pred$mid,
                       upr.p=ci_pred$upr) 

# plot with 95% CI ----
p <- ggplot(cbind(
  data.frame(time=1:n_so2,so2=so2),ci_state),
  aes(x=time,y=so2)) + 
  geom_point(shape=21,color="black",fill="yellow") + 
  geom_line(aes(y=mid),size=.85,color="red") + 
  geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=.45,fill="blue") +
  geom_ribbon(aes(ymin=lwr.p,ymax=upr.p),alpha=.25,fill="blue")

plot(p)

filename <- "output/model/model-D4_so2_pred.png"
png(filename)
plot(p)
dev.off()

# model diagnostic ----
y_t <- so2
mu_t <- mcmc_summary$mean[1:n]
sigma_t <- mcmc_summary$mean[(n+6):(2*n+5)]
xi_t <- rep(mcmc_summary$mean[n+1],n)

z_t <- (1/xi_t) * log(1 + xi_t*((y_t-mu_t)/sigma_t)) # standardised Gumbel 

filename <- "output/model/model-D4.png"
png(filename)
dPlotGumbel(z_t)
dev.off()

