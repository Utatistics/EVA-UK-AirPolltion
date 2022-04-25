# packages 
library(rstan)
library(forecast)
library(tidyverse)
library(gridExtra)

# import dataset 
path <- "C:/Users/frusc/OneDrive/Desktop/DataScience/VM/Ubuntu/export/"
database <- read.csv(paste0(path,"nox_db.csv"),header=TRUE)

# Basic EDA 
A3 <- database %>%
  filter(site_id == 'A3') %>% 
  mutate(date = as.Date(date))

g <- ggplot(A3, aes(x=date,y=nox)) + geom_line()
plot(g)

# 
