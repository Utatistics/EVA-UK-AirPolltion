# packages 
library(extRemes)
library(tidyverse)
library(gridExtra)

# diagnostic functions ----
getrlpoints <- function(z_t){ # obtaining return levels 
  n <- length(z_t)
  xp2 <- ppoints(n=n,a=0)
  z <- sort(z_t) # sorted data 
  rlpoints.x <- -1/log(xp2) # see p.81, Cole(2001)
  rlpoints.y <- z
  return(data.frame(rlpoints.x,rlpoints.y)) # rlpoints
}

# diagnostic plots: standard Gumbel  model ----
dPlotGumbel <- function(z_t){
  z <- z_t
  n <- length(z_t)
  qgp <- qevd(p=c(.01,.90), # Gumbel quantile points 
              loc=0,
              scale=1,
              shape=0,
              type="GEV")
  rlpoints <- getrlpoints(z) %>% filter(rlpoints.x>=2)
  rperiods = c(2,5,10,20,50,80,100,120,200,250,300,500,800)
  gumbel <- revd(n=3e5,loc=0,scale=1,shape=0,type="GEV")
  cat("fitting dummy model...\n")
  fit <- fevd(gumbel)
  cat("completed!\n")
  bds <- ci(fit,return.period=rperiods) # gumberl return level plot with CI
  ci_df <- data.frame(c1=as.numeric(bds[,1]),
                      c2=as.numeric(bds[,2]),
                      c3=as.numeric(bds[,3]),
                      rperiods=rperiods)

  g <- ggplot(data.frame(month=1:n,z=z),
              aes(x=month,y=z)) + 
    geom_point(shape=21,colour="black",fill="blue") 

  dd <- ggplot(data.frame(z=z),aes(x=z)) + 
    geom_density(color="black",fill="blue",alpha=.65) +
    stat_function(data=data.frame(qq=seq(qgp[1],qgp[2],len=100)),
                  aes(x=qq),
                  fun=devd,
                  args=list(loc=0,
                            scale=1,
                            shape=0,
                            type="GEV"),
                  linetype="dashed",color="yellow",
                  size=.75) + 
    labs(y="density")

  qq <- ggplot(data.frame(z=sort(z),
                          q=qevd(p=ppoints(n=n,a=0), # generating points needed for qq plot
                                 loc=0,
                                 scale=1, # extracting the parameters 
                                 shape=0,
                                 type="GEV")),
               aes(x=q,y=z)) +
    geom_point(shape=21,color="black",fill="blue") + 
    geom_abline(intercept=0,slope=1,linetype="dashed") + 
    labs(x="Theoretical",y="Sampled")
  
  zz <- ggplot() +
    geom_line(data=ci_df,aes(x=rperiods,y=c2),color="red") +
    geom_line(data=ci_df,aes(x=rperiods,y=c1),color="black",linetype="dashed") +
    geom_line(data=ci_df,aes(x=rperiods,y=c3),color="black",linetype="dashed") +
    geom_point(data=rlpoints,aes(x=rlpoints.x,y=rlpoints.y),shape=21,color="black",fill="blue") +
    ylab("return level") + # m
    xlab("return period") + # years 
    scale_x_log10(expand=c(0,0),breaks=c(2,5,10,20,50,100,200,500)) 
  
  grid.arrange(g,dd,qq,zz)
}