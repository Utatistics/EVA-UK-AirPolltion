# packages 
library(tidyverse)
library(gridExtra)

# diagnostic functions ----
getrlpoints <- function(model){ # obtaining return levels 
  xp2 <- ppoints(n=model$n,a=0)
  data <- datagrabber(model) 
  y <- c(data[,"y"]) %>% sort() # sorted data 
  rlpoints.x <- -1/log(xp2) # see p.81, Cole(2001)
  rlpoints.y <- y
  return(data.frame(rlpoints.x,rlpoints.y)) # rlpoints
}

getcidf <- function(model){ # get CI for return level
  rperiods = c(2,5,10,20,50,80,100,120,200,250,300,500,800)
  bds <- ci(model,return.period=rperiods) # find confidence intervals *see "distillery"
  c1 <- as.numeric(bds[,1])
  c2 <- as.numeric(bds[,2])
  c3 <- as.numeric(bds[,3])
  return(data.frame(c1,c2,c3,rperiods)) # ci-df
}

# diagnostic plots: GEV model ----
dPlotGEV <- function(model){
  y <- model$x
  n <- model$n
  ci_df <- getcidf(model)
  rlpoints <- getrlpoints(model) %>% filter(rlpoints.x>=2)
  params <- distill(model) # extracing the parameters 
  qgp <- qevd(p=c(.01,.90), # GEV quantile points 
              loc=params["location"],
              scale=params["scale"],
              shape=params["shape"],
              type="GEV")
  
  g <- ggplot(data.frame(month=1:n,y=y),
                       aes(x=month,y=y)) + 
    geom_point(shape=21,colour="black",fill="blue") 
    
  dd <- ggplot(data.frame(y=y),aes(x=y)) + # conditioned on y > u
    geom_density(color="black",fill="blue",alpha=.65) +
    stat_function(data=data.frame(z=seq(qgp[1],qgp[2],len=100)),
                  aes(x=z),
                  fun=devd,
                  args=list(loc=params["location"],
                            scale=params["scale"],
                            shape=params["shape"],
                            type="GEV"),
                  linetype="dashed",color="yellow",
                  size=.75) + 
    labs(y="density")
    
  
  qq <- ggplot(data.frame(y=sort(y),
                          q=qevd(p=ppoints(n=n,a=0), # generating points needed for qq plot
                                 loc=params["location"],
                                 scale=params["scale"], # extracting the parameters 
                                 shape=params["shape"],
                                 type="GEV")),  # GP has 2 parameters!! *uniquely determined by corresponding GEV params 
               aes(x=q,y=y)) +
    geom_point(shape=21,color="black",fill="blue") + 
    geom_abline(intercept=0,slope=1,linetype="dashed") + 
    labs(x="Theoretical",y="Sampled")
  
  z <- ggplot() +
    geom_line(data=ci_df,aes(x=rperiods,y=c2),color="red") +
    geom_line(data=ci_df,aes(x=rperiods,y=c1),color="black",linetype="dashed") +
    geom_line(data=ci_df,aes(x=rperiods,y=c3),color="black",linetype="dashed") +
    geom_point(data=rlpoints,aes(x=rlpoints.x,y=rlpoints.y),shape=21,color="black",fill="blue") +
    ylab("return level") + # m
    xlab("return period") + # years 
    scale_x_log10(expand=c(0,0),breaks=c(2,5,10,20,50,100,200,500)) 
  
  grid.arrange(g,dd,qq,z)
}
