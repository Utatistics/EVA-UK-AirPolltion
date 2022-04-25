# packages 
library(openair) # https://bookdown.org/david_carslaw/openair/
library(tidyverse)

# import data ----
aurn_meta <- importMeta(source="aurn") # WHAT SITES ARE AVAILABLE ?

# site = 'London N. "Kensington"'
kc <- importAURN(site="KC1",year=1996:2021,meta=TRUE) %>% 
  mutate(Date=date) %>% 
  mutate(date=as.POSIXct(date)) %>% 
  mutate(year=format(date,"%Y"),
         month=format(date,"%m"),
         date=format(date,"%d"),
         time=str_extract_all(Date,pattern="\\d{2}:.+\\d{2}") %>% unlist()) %>% 
  select("site","code","latitude","longitude","site_type", # general info for obs
         "year","month","date","time", # time labels
         "co","nox","no2","no","o3","so2","pm10","pm2.5", # pollutants observed 
         "ws","wd","air_temp") # covariates

glimpse(kc)
write.csv(kc,file='data/kc.csv') 

list.files("data")
kc <- read.csv("data/kc.csv") %>% as 