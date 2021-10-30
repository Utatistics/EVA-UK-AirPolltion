# packages 
library(evd)
library(ismev)
library(extRemes)
library(tidyverse)

# sample extreme rv ----
mu <- .56
sigma <- 1.01
n <- 1e3
M <- 365

uv <- rextreme(n=1e3,quantfun=qnorm,mean=mu,sd=sigma,mlen=M) # mlen = size of block
bm <- rep(NA,n)
for (i in 1:n){
  bm[i] <- max(rnorm(M,mu,sigma))
}
uvdata <- data.frame(t=1:n,uv=uv,bm=bm) %>% pivot_longer(!t,names_to="name",values_to="value")

g <- ggplot(data.frame(t=1:n,uv=uv),aes(x=t,y=uv)) + geom_line()
p <- ggplot(data.frame(t=1:n,uv=uv),aes(x=t,y=uv)) + geom_point(fill="blue",colour="black",shape=21) 
h <- ggplot(uvdata,aes(x=value,color=name)) + geom_density(alpha=.5)

plot(g)
plot(p)
plot(h)

# block maxima/minima model: MLE ----
mm.n <- fextreme(uv,start=list(mean=0,sd=1),distn="norm",mlen=365)
mm.exp <- fextreme(uv,start=list(rate=1),distn="exp",mlen=365,
                   method="Brent",lower=.01,upper=10)
mm.ga <- fextreme(uv,start=list(scale=1),shape=1,distn="gamma",mlen=365, # fixed shape
                  method="Brent",lower=.01,upper=10)
mm.ga2 <- fextreme(uv,start=list(scale=1,shape=1),distn="gamma",mlen=365)

# GEV ----
n <- 1e3
uvdata <- data.frame(t=1:n,
                     uv=rgev(n,loc=.13,scale=1.1,shape=.2)) # random sampling from GEV i.e. maxima
trend <- (-499:500)/n
gev <- fgev(uvdata$uv)
gev1 <- fgev(uvdata$uv,nsloc=trend,control=list(trace=1))
gev2 <- fgev(uvdata$uv,shape=0)
gev3 <- fgev(uvdata$uv,scale=1,shape=0)
anova(gev1,gev2,gev3)

# plot diagnostics ---- 
a <- gev$param["loc"] %>% unname()  
b <- gev$param["scale"] %>% unname()
s <- gev$param["shape"] %>% unname()
t <- seq(.1,n,length=n)
yt <- -log(1-(1/t))^-1
m.gev <- function(z){dgev(z,loc=a,scale=b,shape=s)} # GEV distribution function with MLE
m.q <- qgev(p=c(.01,.999),loc=a,scale=b,shape=s) # GEV quantile 

# plot data.frame ----
PQ <- data.frame(Empirical=(1:n)/(n+1),
                 Model=pgev(sort(uvdata$uv),
                            loc=a,scale=b,shape=s))
QQ <- data.frame(Empirical=sort(uvdata$uv),
                 Model=qgev((1:n/(n+1)), # quantile
                            loc=a,scale=b,shape=s))

# ggplot obj ----
d <- ggplot(uvdata,aes(x=uv)) +
  geom_density(fill="blue",color="black",alpha=.65) + 
  stat_function(data=data.frame(x=seq(m.q[1],m.q[2],len=100)),
                aes(x=x),
                fun=m.gev,
                linetype="dashed",color="yellow",size=.75) 
p <- ggplot(PQ,aes(x=Empirical,y=Model)) +
  geom_point(shape=21,color="black",fill="limegreen") +
  geom_abline(intercept=0,slope=1,linetype="dashed")
q <- ggplot(QQ,aes(x=Empirical,y=Model)) +
  geom_point(shape=21,color="black",fill="green") +
  geom_abline(intercept=0,slope=1,linetype="dashed")

# RETURN PLOT!! ----
f <- function(z){1/(1-exp(-exp(-z)))}
Z <- data.frame(t=t, # return period
                yt=yt, 
                zt=a + b*(yt^s - 1)/s, # return level 
                u=NA, # upper bound
                l=NA, # lower bound
                pp=(-1/log(ppoints(n))), # ???
                uv=uvdata$uv %>% sort()) # observed extreme values 
# A
z <- ggplot(Z,aes(x=(log(yt)),y=zt)) + 
  geom_line(color="black") +
  labs(x="log(y)",y="Return Levels")
# B
z <- ggplot(Z,aes(x=log(yt),y=zt)) +
  geom_line(color="black") +
  geom_point(aes(x=pp,y=uv)) + 
  labs(x="log(y)",y="Return Levels")
# C
z <- ggplot(Z,aes(x=t,y=uv)) + 
  geom_point(shape=21,color="black",fill="green") + 
  geom_line(aes(x=t,y=zt),color="black") + 
  labs(x="Return Period",y="Return Levels")

View(data.frame(t=t,
                lny=log(yt),
                flny=f(log(yt)),
                z=Z$zt,
                fz=f(Z$zt)))

# plotting results---- 
plot(d) # density plot 
plot(p) # probability plot
plot(q) # quantile plot 
plot(z) # return level plot

plot(gev) # built-in diagnostics 
