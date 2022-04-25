# packages 
library(tidyverse)
library(gridExtra)

# diagnostic functions ----
getrlpoints <- function(model){ # obtaining return levels 
  xp2 <- ppoints(n=model$n,a=0)
  data <- datagrabber(model) 
  y <- c(data[,"y"]) %>% sort() # sorted data 
  npy <- model$npy # npy: number of points per period *2.06
  u <- model$threshold # threshold (= 6)
  rlpoints.x <- -1/log(xp2)[y>u]/npy # see p.81, Cole(2001)
  rlpoints.y <- y[y>u]
  return(data.frame(rlpoints.x,rlpoints.y)) # rlpoints
}
getcidf <- function(model){ # get CI for return level
  rperiods = c(2,5,10,20,50,80,100,120,200,250,300,500,800)
  bds <- ci(model,return.period=rperiods) # find confidence intervals *see "distillery"
  c1 <- as.numeric(bds[,1]) # lower
  c2 <- as.numeric(bds[,2]) # middle
  c3 <- as.numeric(bds[,3]) # upper
  return(data.frame(c1,c2,c3,rperiods)) # ci-df
}

# diagnostic plots: POT model ----
dPlotPOT <- function(model){
  x <- datagrabber(model) %>% c()
  n <- model$n
  u <- model$threshold
  y <- x[x>u] # exceedances
  ci_df <- getcidf(model)
  rlpoints <- getrlpoints(model) %>% filter(rlpoints.x>=2)
  params <- distill(model) # extracing the parameters 
  gqp <- qevd(p=c(.01,.90), # GP quantile points 
              threshold=u,
              scale=params["scale"],
              shape=params["shape"],
              type="GP")
  
  p <- ggplot(data.frame(time=1:n,
                         y=x,
                         excess=ifelse(x > u,0,1)),
              aes(x=time,y=y,fill=factor(excess))) +
    geom_point(shape=21,color="black",alpha=.65) +
    geom_hline(yintercept=u,linetype="dashed") +
    scale_fill_discrete(name="excess",labels=c("TRUE","FALSE")) +
    theme(plot.title=element_text(hjust=0.5),
          legend.position=c(.85,.85),
          legend.direction="vertical",
          legend.background=element_rect(fill=alpha("white",.65))) 
  
  dd <- ggplot(data.frame(ex=y),aes(x=ex)) + # conditioned on y > u
    geom_density(color="black",fill="blue",alpha=.65) +
    stat_function(data=data.frame(z=seq(gqp[1],gqp[2],len=100)),
                  aes(x=z),
                  size=.75,
                  linetype="dashed",color="yellow",
                  fun=dgpd,
                  args=list(loc=u,
                            scale=params["scale"],
                            shape=params["shape"])) +
    labs(x="exceedance",y="density")
  
  
  qq <- ggplot(data.frame(y=sort(y),
                          q=qevd(p=ppoints(n=length(y),a=0), # generating points needed for qq plot
                                 scale=params["scale"], # extracting the parameters 
                                 shape=params["shape"],
                                 threshold=u,
                                 type="GP")),  # GP has 2 parameters!! *uniquely determined by corresponding GEV params 
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
  
  grid.arrange(p,dd,qq,z)
}

