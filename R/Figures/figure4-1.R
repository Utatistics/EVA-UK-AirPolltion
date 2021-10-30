# packages 
library(tidyverse)
library(gridExtra)

# mrl plot: POT model ----
f <- function(x,xlab){
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
    labs(x=xlab,y="mean excess") 
  
  return(m)
}

# get data ----
kc <- read.csv("data/kc.csv")

so2 <- kc %>%
  drop_na(so2) %>%
  filter(so2!=0) %>%
  select(so2) %>%
  mutate(so2=log(so2)) %>% # log
  drop_na() %>% # NaNs produced *e.g. log(0)
  sapply(as.numeric) %>% # convert to matrix
  c() %>%
  diff() # 1st order differencing

o3 <- kc %>% 
  drop_na(o3) %>%
  filter(o3!=0) %>% 
  select(o3) %>% 
  sapply(as.numeric) %>% # convert to matrix
  c() 

# Figure 4.1 ----
m1 <- f(so2,"u: SO2")
m2 <- f(o3, "u: O3")

filename <- "output/figure/fig4-1.png"
png(filename)
grid.arrange(m1,m2)
dev.off()
