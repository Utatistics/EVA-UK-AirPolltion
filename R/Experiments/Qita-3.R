# packages 
library(evir)
library(ismev)
library(extRemes)
library(tidyverse)

# reference ----
url <- "https://qiita.com/hrkz_szk/items/8b01552e0e09583d4c35"

# get data ----
data("dowjones")
str(dowjones)
return <- diff(log(dowjones$Index))*100
loss  <- -return
u <- 2 # threshold chosen 

g <- ggplot(dowjones,aes(x=Date,y=Index)) + 
  geom_line()

p <- ggplot(data.frame(x=dowjones$Date[-1],y=loss),
            aes(x=x,y=y))+ 
  geom_line() + 
  labs(x="year",y="loss")

pot <- ggplot(data.frame(x=dowjones$Date[-1],
                         y=loss,
                         excess=ifelse(loss>u,0,1)),
              aes(x=x,y=y,fill=factor(excess))) +
  geom_point(shape=21,color="black") +
  geom_hline(yintercept=u,linetype="dashed") +
  scale_fill_discrete(name="excess",labels=c("TRUE","FALSE"))

plot(g)
plot(p)
plot(pot)

# choice of threshold: mean residual life plot ----
us <- seq(min(loss),max(loss),.01)
mean.excess <- vector("numeric",length(us)) # generate empty vector 

for(i in 1:length(mean.excess)){
  threshold.exceedances <- return[return > us[i]]
  mean.excess[i] <- mean(threshold.exceedances - us[i]) # sample mean for threshold exceedance 
}

m <- ggplot(data.frame(u=us,mean=mean.excess),
            aes(x=us,y=mean)) + 
  geom_line()

plot(m) # mean residual life plot 
mrl.plot(loss) # built-in *with confidence interval

# choice of threshold: standard error  ----
gpd.fitrange(return,umin=0,umax=3,nint=31) # u = [0,3] and 31 fitted models

n.exc <- length(loss[loss > 2]) # choice of threshld: 2
n.all <- length(loss)
ex.rate <- n.exc / n.all # exceedance rate 

# peak-over threshold model: ismev & extRemes ----
fit.e <- extRemes::fevd(loss,threshold=u,type="GP")
ci.e <- ci.fevd(fit.e,type="parameter",which.par=2,method="proflik") # WARNINGS!! *shape parameter

fit.i <- ismev::gpd.fit(loss,threshold=u) 
ci.i <- data.frame(low=fit.i$mle - 1.96*fit.i$se, # confidence interval 
                  upper=fit.i$mle + 1.96*fit.i$se,
                  row.names=c("scale","shape"))

gpd.prof(fit.i,m=10,xlow=5,xup=50) # 
gpd.prof(fit.i,m=100,xlow=7,xup=200,nint=1000)
gpd.diag(fit.i)
