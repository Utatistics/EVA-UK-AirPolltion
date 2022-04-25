# packages
library(tidyverse)
source("R/Functions/function-2.R")

# get data  ----
vars <- c("co","nox","no2","no","o3","so2","pm10","pm2.5") # names of pollution 
df <- read.csv("data/kc.csv") # site = "London N. "Kensington"

# time series scatter plot ---- 
for(var in vars){ 
  d <- monthly(df,var) %>% drop_na(any_of(var))
  n <- nrow(d)
  print(c(var,n))

  g <- ggplot(d,aes_string(x=1:n,y=var)) +
    geom_point(shape=21,color="black",fill="blue") + 
    labs(x="time")
  plot(g)
  
  filename <- paste0("output/scatterplot/",var,".png")
  png(filename)
  plot(g)
  dev.off()
}

# scater plot against wind speed ----
for(var in vars){ 
  d <- monthly(df,var)
  n_miss <- is.na(d$ws) %>% sum()
  print(c(var,n_miss))
  
  w <- ggplot(d,aes_string(x="ws",y=var)) + 
    geom_point(shape=21,color="black",fill="green")
  plot(w)
  
  filename <- paste0("output/windspeed/",var,"-wind.png")
  png(filename)
  plot(w)
  dev.off()
}

# scater plot against air temperature ----
for(var in vars){ 
  d <- monthly(df,var)
  n_miss <- is.na(d$air_temp) %>% sum()
  print(c(var,n_miss))
  
  w <- ggplot(d,aes_string(x="air_temp",y=var)) + 
    geom_point(shape=21,color="black",fill="skyblue")
  plot(w)
  
  filename <- paste0("output/airtemp/",var,"-aritemp.png")
  png(filename)
  plot(w)
  dev.off()
}

# time series plot ----
for (var in vars){
  y <- monthly(df,var) %>% 
    ungroup() %>% 
    select(as.symbol(var)) %>% 
    sapply(as.numeric) %>% 
    c() 

  g <- ggplot(data.frame(time=1:length(y),diff=y),
              aes(x=time,y=diff)) + 
    geom_line() + 
    labs(y=var)
  plot(g)
  
  filename <- paste0("output/differenced0/",var,".png")
  png(filename)
  plot(g)
  dev.off()
}

# time series plot *log ----
for (var in vars){
  y <- monthly(df,var) %>% 
    ungroup() %>% 
    select(as.symbol(var)) %>% 
    sapply(as.numeric) %>% 
    c() %>% 
    log()
  
  g <- ggplot(data.frame(time=1:length(y),diff=y),
              aes(x=time,y=diff)) + 
    geom_line() + 
    labs(y=paste0("ln(",var,")"))
  plot(g)
  
  filename <- paste0("output/log-differenced0/",var,".png")
  png(filename)
  plot(g)
  dev.off()
}

# time series plot *1st order differencing ----
for (var in vars){
  y <- monthly(df,var) %>% 
    ungroup() %>% 
    select(as.symbol(var)) %>% 
    sapply(as.numeric) %>% 
    c() %>% 
    diff() # differenced maximas
  
  g <- ggplot(data.frame(time=1:length(y),diff=y),
              aes(x=time,y=diff)) + 
    geom_line() + 
    labs(y=paste0(var," *d=1"))
  plot(g)
  
  filename <- paste0("output/differenced1/",var,".png")
  png(filename)
  plot(g)
  dev.off()
}

# time series plot *log & 1st order differencing ----
for (var in vars){
  y <- monthly(df,var) %>% 
    ungroup() %>% 
    select(as.symbol(var)) %>% 
    sapply(as.numeric) %>% 
    c() %>% 
    log %>% 
    diff() # differenced maximas
  
  g <- ggplot(data.frame(time=1:length(y),diff=y),
              aes(x=time,y=diff)) + 
    geom_line() + 
    labs(y=paste0("ln(",var,") *d=1"))
  plot(g)
  
  filename <- paste0("output/log-differenced1/",var,".png")
  png(filename)
  plot(g)
  dev.off()
}
