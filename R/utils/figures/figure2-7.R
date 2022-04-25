# packages 
library(tidyverse)
library(gridExtra)
source("R/Functions/function-2.R")

# get data ----
kc <- read.csv("data/kc.csv")

o3 <- monthly(kc,"o3") %>%
  ungroup() %>% 
  select(o3) %>% 
  sapply(as.numeric) %>% 
  c() 

Date <- monthly(kc,"o3") %>% 
  ungroup() %>% 
  select(Date)
  

temp <- monthly(kc,"o3") %>%
  ungroup() %>% 
  select(air_temp) %>% 
  sapply(as.numeric) %>% 
  c() 

temp_imp <- monthly(kc,"o3") %>% # mean imputation 
  ungroup() %>% 
  select(month,air_temp) %>% 
  group_by(month) %>%
  mutate(air_temp_imp = replace(air_temp,
                                is.na(air_temp),
                                mean(air_temp,na.rm=TRUE))) %>% 
  ungroup() %>% 
  select(air_temp_imp) %>% 
  sapply(as.numeric) %>% 
  c() 

# Figure 2-7 ----
g <- ggplot(data.frame(x=Date$Date,y=temp_imp),aes(x=x,y=y)) +
  geom_line() + 
  labs(x="time",y="air temperature") + 
  geom_ribbon(aes(ymin=-Inf,
                  ymax=ifelse(is.na(temp)==TRUE,Inf,-Inf)),
              fill="blue",alpha=.15)

filename <- "output/figure/fig2-7.png"
png(filename)
plot(g)
dev.off()

