# packages 
library(rstan)
library(tidyverse)
library(gridExtra)

# monthly cycle 
T = 12
N = T/2
w <- pi/N
t <- 1:300
a <- rnorm(6)
b <- rnorm(6)

fourie <- data.frame(t=t,
                     cos1=a[1]*cos(1*w*t),
                     sin1=b[1]*sin(1*w*t),
                     cos2=a[2]*cos(2*w*t),
                     sin2=b[2]*sin(2*w*t),
                     cos3=a[3]*cos(3*w*t),
                     sin3=b[3]*sin(3*w*t),
                     cos4=a[4]*cos(4*w*t),
                     sin4=b[4]*sin(4*w*t),
                     cos5=a[5]*cos(5*w*t),
                     sin5=b[5]*sin(5*w*t),
                     cos6=a[6]*cos(6*w*t),
                     sin6=b[6]*sin(6*w*t)) %>% round(digits=3)

fourie$sum <- rowSums(fourie[,-1]) 
fourie %>% distinct(sum,.keep_all=TRUE)