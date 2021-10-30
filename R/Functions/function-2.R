# packags 
library(tidyverse)

# monthly maxima ----
monthly <- function(df,name){ # variable name "str"
  pol <- df %>%
    group_by(year,month) %>% 
    slice_max(eval(as.name(name))) %>% 
    distinct(eval(as.name(name)),.keep_all=TRUE) %>% #referring to R obj by "str"
    mutate(Date=as.POSIXct(Date)) %>% 
    select("site","code","latitude","longitude","site_type", # general info for obs
           "Date","year","month","date","time", # time labels
           name, # pollutants observed 
           "ws","wd","air_temp") # covariates
  return(pol)
}

# annual maxima ----
annual <- function(df,name){ # variable name "str"
  pol <- df %>%
    group_by(year) %>% 
    slice_max(eval(as.name(name))) %>% 
    distinct(eval(as.name(name)),.keep_all=TRUE) %>% #referring to R obj by "str"
    select("site","code","latitude","longitude","site_type", # general info for obs
           "year","month","date","time", # time labels
           name, # pollutants observed 
           "ws","wd","air_temp") # covariates
  return(pol)
}

# residual diagnostics: error anlysis ----
rPlot <- function(model){
  resid <- residuals(model,standardize=TRUE)
  n <- length(resid)
  
  print("plotting...(1)") %>% as.symbol()
  g1 <- ggplot(data.frame(t=1:n,resid=resid),
               aes(x=t,y=resid)) + 
    geom_point(shape=21,color="black",fill="skyblue") + 
    theme_minimal()
  
  print("plotting...(2)") %>% as.symbol()
  g2 <- ggAcf(resid) + 
    ggtitle("") + 
    theme_minimal()
  
  print("plotting...(3)") %>% as.symbol()
  g3 <- ggplot(data.frame(resid=resid),
               aes(x=resid,y=..density..)) +
    geom_histogram(fill="blue",colour="black",alpha=.65) +
    geom_density(colour="magenta",size=1)  +
    theme_minimal()
  
  print("plotting...(4)") %>% as.symbol()
  g4 <- ggplot(data.frame(resid=resid),
               aes(sample=resid)) +
    stat_qq_band(distribution="norm",
                 dparams=list(mean=0,sd=1)) +
    stat_qq_line(distribution="norm",
                 dparams=list(mean=0,sd=1)) +
    stat_qq_point(distribution="norm",
                  dparams=list(mean=0,sd=1),
                  shape=21,colour="black",fill="limegreen") +
    labs(x="Theoretical",y="Sample") + 
    theme_minimal()
  
  grid.arrange(g1,g2,g3,g4)
}
