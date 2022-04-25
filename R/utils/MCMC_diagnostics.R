# packages 
library(rstan)
library(tidyverse)

# model-D3 ----
load(file="data/mcmc-model-D3.RData")
stan_dens(mod.gev,pars=c("mu","sigma","xi"))
stan_trace(mod.gev,pars=c("mu","sigma","xi"),inc_warmup=TRUE) + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

filename <- "output/traceplot/traceplot-D3.png"
png(filename)
stan_trace(mod.gev,pars=c("mu","sigma","xi"),inc_warmup=TRUE) + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
dev.off()

rm(list=ls())
# model-D4 ----
load(file="data/mcmc-model-D4.RData")

stan_dens(mod.gev,pars=c("mu","gamma","beta1","beta2"))
stan_trace(mod.gev,pars=c("xi","gamma","beta1","beta2"),inc_warmup=TRUE) + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

filename <- "output/traceplot/traceplot-D4.png"
png(filename)
stan_trace(mod.gev,pars=c("xi","gamma","beta1","beta2"),inc_warmup=TRUE) + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
dev.off()

rm(list=ls())

# model-D4S1 ----
load(file="data/mcmc-model-D4S1.RData")
stan_trace(mod.gev_s,pars=c("sigma","xi","gamma_xi","gamma_s"),inc_warmup=TRUE) + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

filename <- "output/traceplot/traceplot-D4S1.png"
png(filename)
stan_trace(mod.gev_s,pars=c("sigma","xi","gamma","gamma_s"),inc_warmup=TRUE) + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
dev.off()

rm(list=ls())
# model-D4S2 ----
load(file="data/mcmc-model-D4S2.RData")

stan_trace(mod.gev_s,pars=c("sigma","xi","gamma","gamma_s"),inc_warmup=TRUE) + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

filename <- "output/traceplot/traceplot-D4S2.png"
png(filename)
stan_trace(mod.gev_s,pars=c("sigma","xi","gamma","gamma_s"),inc_warmup=TRUE) + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
dev.off()

rm(list=ls())

# model-D3S3 ----
load(file="data/mcmc-model-D4S3.RData")

stan_trace(mod.gev_s,pars=c("sigma","xi","gamma"),inc_warmup=TRUE) + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

filename <- "output/traceplot/traceplot-D4S3.png"
png(filename)
stan_trace(mod.gev_s,pars=c("sigma","xi","gamma"),inc_warmup=TRUE) + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
dev.off()

rm(list=ls())

# model-D4S4 ----
load(file="data/mcmc-model-D4S4.RData")

stan_trace(mod.gev_s,pars=c("sigma","xi","gamma","beta"),inc_warmup=TRUE) + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

filename <- "output/traceplot/traceplot-D4S4.png"
png(filename)
stan_trace(mod.gev_s,pars=c("sigma","xi","gamma","beta"),inc_warmup=TRUE) + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
dev.off()

rm(list=ls())

# model-D4S5 ----
load(file="data/mcmc-model-D4S5.RData")

stan_trace(mod.gev_s,pars=c("sigma","xi","gamma","beta"),inc_warmup=TRUE) + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

filename <- "output/traceplot/traceplot-D4S5.png"
png(filename)
stan_trace(mod.gev_s,pars=c("sigma","xi","gamma","beta"),inc_warmup=TRUE) + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
dev.off()

rm(list=ls())