# packages 
library(rstan)
library(forecast)
library(tidyverse)
library(gridExtra)
source("R/Functions/function-2.R")
source("R/Functions/function-7.R")

# get data ----
kc <- read.csv("data/kc.csv")
load(file="data/beta.RData")

o3 <- monthly(kc,"o3") %>%
  ungroup() %>% 
  select(o3) %>% 
  sapply(as.numeric) %>% 
  c() 

temp <- monthly(kc,"o3") %>%
  ungroup() %>% 
  select(air_temp) %>% 
  sapply(as.numeric) %>% 
  c() 

temp_imp <- monthly(kc,"o3") %>% # mean imputation 
  ungroup() %>% 
  select(month,air_temp) %>% 
  group_by(month) %>%
  mutate(air_temp_imp = replace(air_temp,
                                is.na(air_temp),
                                mean(air_temp,na.rm=TRUE))) %>% 
  ungroup() %>% 
  select(air_temp_imp) %>% 
  sapply(as.numeric) %>% 
  c() 

month <- monthly(kc,"o3") %>%
  ungroup() %>% 
  select(month) %>% 
  sapply(as.numeric) %>% 
  c() 

n <- length(o3)

M <- matrix(NA,n,12)
for (i in 1:n){
  for (j in 1:12){
   if (j==month[i])
     M[i,j] <- 1
   else 
     M[i,j] <- 0
  } 
}

# state space model: GEV ----
sample_iterations <- 5e3
data_s <-list(N=n,
              y=o3,
              x=temp_imp,
              M=M,
              p=2,
              beta=fit$coefficients %>% unname(),
              gamma_s=sigma(fit),
              X=data.frame(intercept=rep(1,n),
                           x=temp_imp))

mod.gev_s <- stan(file='stan/model-D4S_sample4.stan',
                  data=data_s,
                  seed=1,
                  chains=4,
                  init_r=2,
                  iter=sample_iterations,
                  warmup=sample_iterations/5)

# storing the samplings ----
ms_s <- rstan::extract(mod.gev_s)
mcmc_s <- data.frame(loc=ms_s$mu,
                     scale=ms_s$sigma,
                     shape=ms_s$xi,
                    # mean=ms_s$Mean,
                     beta=ms_s$beta,
                     yrep=ms_s$yrep)

mcmc_summary <- summary(mod.gev_s)[["summary"]] %>% as.data.frame() 

save(mod.gev_s,ms_s,mcmc_summary,mcmc_s,file="data/mcmc-model-D4S4.RData")
load("data/mcmc-model-D4S4.RData")

# diagnostic plots ----
stan_dens(mod.gev_s,pars=c("sigma","xi","gamma","gamma_s"),inc_warmup=FALSE) 
stan_trace(mod.gev_s,pars=c("sigma","xi","gamma","beta"),inc_warmup=TRUE) + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

# credible interval ----
ci_s <- apply(X=mcmc_s, 
              MARGIN=2, # apply to columns 
              FUN=quantile,
              probs=c(.025,.5,.975)) %>% # augument of FUN: # 95% CI
  t() %>% 
  as.data.frame() 

colnames(ci_s) <- c("lwr","mid","upr")

ci_loc <- ci_s[1:n,]
ci_scale <- ci_s[n+1,]
ci_shape <- ci_s[n+2,]
ci_pred <- tail(ci_s,n=n) # μ+σ(g1−1)/ξ
ci_state <- data.frame(lwr=ci_loc$lwr + ci_scale$lwr*(gamma(1-ci_shape$lwr)-1)/ci_shape$lwr,
                       mid=ci_loc$mid + ci_scale$mid*(gamma(1-ci_shape$mid)-1)/ci_shape$mid,
                       upr=ci_loc$upr + ci_scale$upr*(gamma(1-ci_shape$upr)-1)/ci_shape$upr,
                       lwr.p=ci_pred$lwr,
                       mid.p=ci_pred$mid,
                       upr.p=ci_pred$upr) 

# plot with 95% CI ----
p_s <- ggplot(cbind(
  data.frame(time=1:n,o3=o3),ci_state),
  aes(x=time,y=o3)) + 
  geom_point(shape=21,color="black",fill="yellow") + 
  geom_line(aes(y=mid.p),size=.85,color="red") + 
  geom_ribbon(aes(ymin=lwr.p,ymax=upr.p),alpha=.25,fill="blue") +
  geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=.45,fill="blue")

plot(p_s)

# saving prediction plots ----
filename <- "output/model/model-D4S4_pred.png"
png(filename)
plot(p_s)
dev.off()

# model diagnostic ----
y_t <- o3
mu_t <- mcmc_summary$mean[5:(n+4)]
sigma_t <- rep(mcmc_summary$mean[1],n)
xi_t <- rep(mcmc_summary$mean[2],n)

z_t <- (1/xi_t) * log(1 + xi_t*((y_t-mu_t)/sigma_t)) # standardised Gumbel 

filename <- "output/model/model-D4S4.png"
png(filename)
dPlotGumbel(z_t)
dev.off()

