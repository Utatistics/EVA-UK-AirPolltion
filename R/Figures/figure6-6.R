# packages 
library(extRemes)
library(ggridges)
library(tidyverse)
source("R/Functions/function-2.R")

# get data ----
kc <- read.csv("data/kc.csv")

o3 <- monthly(kc,"o3") %>%
  ungroup() %>% 
  select(month,o3) 

# shape parameter ----
shape <- rep(NA,12)
s.e <- rep(NA,12)

for (i in 1:12){
  o3.month <- o3 %>% 
    filter(month==i) %>% 
    select(o3) %>% 
    sapply(as.numeric) %>% 
    c()
  
  n <- length(o3.month)

  fit <- fevd(o3.month,type="GEV")
  params <- distill(fit)
  shape[i] <- params["shape"]
  sigma <- parcov.fevd(fit) %>% diag() %>% sqrt()
  s.e[i] <- sigma["shape"]
}

# CLT density ----
n <-1e3
X <- data.frame(x=rep(NA,n*12),month=rep(NA,n*12))
M <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

for (i in 0:11){
  x <- rnorm(n,mean=shape[i+1],sd=s.e[i+1])
  m <- rep(M[i+1],n)
  X[(1:n)+n*i,1] <- x
  X[(1:n)+n*i,2] <- m
}

g <- ggplot(X,aes(x=x,y=month,fill=stat(x))) +
  geom_density_ridges_gradient(scale=3,rel_min_height=0.01) +
  scale_fill_viridis_c(name="shape",option="C") +
  theme(plot.title=element_text(hjust=0.5),
        legend.position=c(.875,.25),
        legend.direction="vertical",
        legend.background=element_rect(fill=alpha("white",.65))) +  
  labs(x="Estimate of shape parameter",y="Month")

# saving the figure ----
filename <- "output/figure/fig6-3.png"
png(filename)
plot(g)
dev.off()
