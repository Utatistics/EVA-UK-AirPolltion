# pacakges 
library(evd)
library(ismev)
library(extRemes)
library(forecast)
library(tidyverse)
library(gridExtra)
source("R/Functions/function-5.R")
source("R/Functions/function-6.R")

# get data ----
kc <- read.csv("data/kc.csv")

o3 <- kc %>% 
  drop_na(o3) %>%
  filter(o3!=0) %>% 
  select(o3) %>% 
  sapply(as.numeric) %>% # convert to matrix
  c() 

n <- length(o3)

# threshold selection----
mrlPlot(o3)
mrlplot(o3)
gpd.fitrange(so2,umin=0,umax=5,nint=30)

filename <- "output/mrlplot/mrlplot-CS.png"
png(filename)
mrlPlot(o3)
dev.off()

u <- 110 # CHOICE OF THRESHOLD!! 
y <- o3[o3 > u] # exceedance 
ex.rate <- length(y) / length(o3) # exceedance rate 

# declustering ----
r <- 24*30 # CHOICE OF CLUSTER!! 
o3.cm <- decluster(o3, # obj = "declusterd"
                   threshold=u,
                   r=r) # run.length 
y.cm <- data.frame(y=y, # exceedance
                   idx=attr(o3.cm,"clusters")) %>% 
  group_by(idx) %>% 
  slice_max(y) %>% # cluster maxima
  ungroup() %>% 
  select(y) %>% 
  sapply(as.numeric) %>% 
  c()

# basic EDA ----
g <- ggplot(data.frame(time=1:n,o3=o3),
            aes(x=time,y=so2)) + 
  geom_line() + 
  labs(y="so2 *d=1")

plot(g)
ggAcf(o3) # auto-correlation *staionary 

p <- ggplot(data.frame(time=1:n,
                       o3=o3,
                       excess=ifelse(o3 > u,0,1)),
            aes(x=time,y=o3,fill=factor(excess))) +
  geom_point(shape=21,color="black") +
  geom_hline(yintercept=u,linetype="dashed") +
  scale_fill_discrete(name="excess",labels=c("TRUE","FALSE")) +
  theme(plot.title=element_text(hjust=0.5),
        legend.position=c(.85,.85),
        legend.direction="vertical",
        legend.background=element_rect(fill=alpha("white",.65))) 

# POT model ----
fit.gp <- fevd(x=o3, # p.42
               threshold=u,
               type="GP") # "GP" for generalized pareto, "GEV" by default 
fit.gp.cm <- fevd(x=o3.cm, # applying GP model to declustered maxima
                  threshold=u,
                  type="GP") 

params <- distill(fit.gp) # extracting parameters 
params.cm <- distill(fit.gp.cm) 

# diagnostic plots ----
filename <- "output/model/model-CS.png"
png(filename)
dPlotPOT(fit.gp) # r=0
dev.off()

filename <- "output/model/model-CS_CM.png"
png(filename)
dPlotPOT(fit.gp.cm) # r=24*30 
dev.off()

# parameters CI ---- 
CI <- data.frame(scale=ci.fevd(fit.gp,type="parameter",which.par=1) %>% c(),
                 scale.cm=ci.fevd(fit.gp.cm,type="parameter",which.par=1) %>% c(),
                 shape=ci.fevd(fit.gp,type="parameter",which.par=2) %>% c(),
                 shape.cm=ci.fevd(fit.gp.cm,type="parameter",which.par=2) %>% c()) %>% t()

H <- fit.gp$results[["hessian"]]

s.e <- parcov.fevd(fit.gp) %>% diag() %>% sqrt()
s.e.cm <- parcov.fevd(fit.gp.cm) %>% diag() %>% sqrt()
