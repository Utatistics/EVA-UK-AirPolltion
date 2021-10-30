# packages 
library(evir)
library(ismev)
library(extRemes)
library(tidyverse)

# reference ----
url <- "https://qiita.com/hrkz_szk/items/43debffda9697d9dd7a9#%E6%8E%A8%E5%AE%9A%E7%B5%90%E6%9E%9C"

# get data ----
data(package="evir") # see all the dataset in package "evir"
data(siemens) # Daily Log Returns on Siemens Share Price
SieLoss <- -siemens*100
SieGEV <- evir::gev(SieLoss,block="semester") # fitting GEV to semester maxima
SieMaxima <- SieGEV$data

g <- ggplot(data.frame(t=attr(SieLoss,"times") %>% format("%Y/%m/%d") %>% as.Date(),y=SieLoss),
            aes(x=t,y=y)) +
  geom_line()

z <- ggplot(data.frame(x=1:SieGEV$n,z=SieMaxima),
            aes(x=x,y=z)) +
  geom_line()

plot(g)
plot(z)

# GEV model: extRemes ====
fit.gev <- fevd(SieMaxima,type="GEV")

# extracting results ----
par.gev <- distill(fit.gev)[1:3] # shape > 0 i.e. Frechet distribution
loglik.gev <- distill(fit.gev)[4]
cov.gev <- matrix(as.numeric(distill(fit.gev)[5:13]),nrow=3,byrow=TRUE, # covariance matrix
                  dimnames=list(c("location","scale","shape"),
                                c("location","scale","shape")))
se.gev <- sqrt(diag(cov.gev)) #standard error

# model diagnostics ----
par(mfrow=c(2,2))
plot.fevd(fit.gev, type = "probprob", main="Probability plot")
plot.fevd(fit.gev, type = "qq", main="Quantile plot")
plot.fevd(fit.gev, type = "rl", main="Return level plot")
plot.fevd(fit.gev, type = "density", main="Density plot")
par(mfrow=c(1,1))

# CI: shape ----
ci.fevd(fit.gev,type="parameter") # length .459
ci.fevd(fit.gev,type="parameter",which.par=3,method="proflik") # length .353 
profliker(fit.gev, # profile likelihood
          type="parameter",
          which.par=3, # i.e. shape parameter
          xrange=c(-0.1,0.5))

# CI: return level ----
ci.fevd(fit.gev,type="return.level",return.period=10) # length 2.879
ci.fevd(fit.gev,type="return.level",return.period=100) # length 12.186
ci.fevd(fit.gev,type="return.level",return.period=10,method="proflik") # ???
ci.fevd(fit.gev,type="return.level",return.period=100,method="proflik") # ???

profliker(fit.gev,
          type="return.level",
          return.period=10,
          xrange=c(4.2,9.0))
profliker(fit.gev,
          type="return.level",
          return.period=100,
          xrange=c(4.2,9.0))

