# packags 
library(extRemes) # reverse dependnt on "distillery"
library(tidyverse)
library(gridExtra)

# reference ----
url <- "https://ulysse-pasquier.com/post/r-plotting-results-from-extremes-package-extreme-value-analysis/"

# get data ----
data("damage",package="extRemes") # importing the "damage" dataset from package "extRemes"
u <- 6 # threshold
y <- damage %>%
  filter(Dam > u) %>%
  select(Dam) %>% # exceedances 
  sapply(as.numeric) %>% # convert to matrix
  c() # convert to numeric

# POT Models ----
fit <- fevd(x=Dam, # p.42
            data=damage,
            threshold=u,
            type="GP", # "GP" for generalized pareto, "GEV" by default 
            time.units="2.06/year") # "m/year" where m is number of points per period/block

params <- distill(fit)

# diagnostics function  ----
dgp <- function(z){ 
  devd(z, # GP distribution function 
       threshold=u,
       scale=params["scale"],
       shape=params["shape"],
       type="GP")}

qgp <- qevd(p=c(.01,.90), # GP quantile points 
            threshold=u,
            scale=params["scale"],
            shape=params["shape"],
            type="GP")

getrlpoints <- function(fit){ # obtaining return levels 
  xp2 <- ppoints(n=fit$n,a=0)
  data <- datagrabber(fit) 
  y <- c(data[,"y"]) %>% sort() # sorted data 
  npy <- fit$npy # npy: number of points per period *2.06
  u <- fit$threshold # threshold (= 6)
  rlpoints.x <- -1/log(xp2)[y>u]/npy # see p.81, Cole(2001)
  rlpoints.y <- y[y>u]
  return(data.frame(rlpoints.x,rlpoints.y)) # rlpoints
}

getcidf <- function(fit){ # get CI for return level
  rperiods = c(2,5,10,20,50,80,100,120,200,250,300,500,800)
  bds <- ci(fit,return.period=rperiods) # find confidence intervals *see "distillery"
  c1 <- as.numeric(bds[,1])
  c2 <- as.numeric(bds[,2])
  c3 <- as.numeric(bds[,3])
  return(data.frame(c1,c2,c3,rperiods)) # ci-df
}

# diagnostic plots ----
g <- ggplot(damage,aes(x=Year,y=Dam)) +
  geom_point(shape=21,color="black",fill="blue") + 
  geom_hline(yintercept=u,linetype="dashed")

d <- ggplot(data.frame(ex=y),aes(x=y)) + # conditioned on y > u
  geom_density(color="black",fill="blue",alpha=.65) +
  stat_function(data=data.frame(z=seq(qgp[1],qgp[2],len=100)),
                aes(x=z),
                fun=dgp,
                linetype="dashed",color="yellow",
                size=.75) 

qq <- ggplot(data.frame(y=sort(y),
                        q=qevd(p=ppoints(n=length(y),a=0), # generating points needed for qq plot
                               scale=params["scale"], # extracting the parameters 
                               shape=params["shape"],
                               threshold=u,
                               type="GP")),  # GP has 2 parameters!! *uniquely determined by corresponding GEV params 
             aes(x=q,y=y)) +
  geom_point(shape=21,color="black",fill="blue") + 
  geom_abline(intercept=0,slope=1,linetype="dashed")

rlpoints <- getrlpoints(fit) 
ci_df <- getcidf(fit)
z <- ggplot() + # return level plot: original 
  geom_line(data=ci_df,aes(x=rperiods,y=c2),color="red") +
  geom_line(data=ci_df,aes(x=rperiods,y=c1),color="black",linetype="dashed") +
  geom_line(data=ci_df,aes(x=rperiods,y=c3),color="black",linetype="dashed") +
  geom_point(data=rlpoints,aes(x=rlpoints.x,y=rlpoints.y),shape=21,color="black",fill="blue") +
  ylab("return level") + # m
  xlab("return period") + # years
  scale_x_log10(expand=c(0,0),breaks=c(2,5,10,20,50,100,200,500))

# diagnostic plots ----
plot(g) # scatter plot 
plot(d) # density plot 
plot(qq) # QQ-plot
plot(z) # return level plot 
grid.arrange(g,d,qq,z)

plot(fit,type="qq") # built-in QQ-plot *model/empirical quantiles not alighned !!
plot(fit,type="rl") # built-in return level plot 
