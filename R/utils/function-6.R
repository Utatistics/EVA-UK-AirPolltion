# packages 
library(tidyverse)
library(gridExtra)

# mrl plot: POT model ----
mrlPlot <- function(x){
  us <- seq(min(x),max(x[x!=max(x)]),.1)
  sigma <- vector("numeric",length(us))  
  mean.excess <- vector("numeric",length(us)) # generate empty vector 
  for(i in 1:length(mean.excess)){
    threshold.exceedances <- x[x > us[i]] 
    n <- length(threshold.exceedances)
    sigma[i] <- sd(threshold.exceedances) / n # CI based on CLT 
    mean.excess[i] <- mean((threshold.exceedances - us[i]),na.rm=TRUE) # sample mean for threshold exceedance 
  }
  
  m <- ggplot(data.frame(u=us,
                         c1=mean.excess - 1.96*sigma,
                         c2=mean.excess,
                         c3=mean.excess + 1.96*sigma),
              aes(x=u,y=c2)) + 
    geom_line(aes(x=u,y=c2),color="red",size=.85) +
    geom_line(aes(x=u,y=c1),color="black",linetype="dashed") +
    geom_line(aes(x=u,y=c3),color="black",linetype="dashed") +
    geom_ribbon(aes(ymin=c1,ymax=c3),alpha=.45,fill="blue") + 
    labs(x="u",y="mean excess")
  
  plot(m) %>% return()
}

