# packages 
library(mice)
library(tidyverse)
source("R/Functions/function-2.R")

# get data ----
kc <- read.csv("data/kc.csv")

temp <- monthly(kc,"o3") %>%
  ungroup() %>% 
  select(month,o3,air_temp,air_temp) 

n <- nrow(temp)

# basic EDA ----
ggplot(temp,aes(x=1:n,y=air_temp)) + geom_line()
ggplot(temp,aes(x=month,y=air_temp)) + geom_point(shape=21,fill="blue",color="black")

# mean imputation  ----
temp_imp <- temp %>% 
  group_by(month) %>%
# mutate(o3=o3-mean(o3)) %>% # centerising 
  mutate(air_temp_imp = replace(air_temp,
                            is.na(air_temp),
                            mean(air_temp,na.rm=TRUE)))
  

ggplot(temp_imp,aes(x=1:n,y=air_temp_imp)) + geom_line()

# quadradic regression ----
fit <- lm(data=temp_imp,o3~air_temp_imp+I(air_temp_imp^2))
X <- model.matrix(fit) %>% as.matrix()
params <- fit$coefficients 
error <- vcov(fit) %>% diag() %>% sqrt()
ci <- data.frame(lwr=X%*%as.matrix(params-1.96*error),
                 upr=X%*%as.matrix(params+1.96*error))

ggplot(cbind(temp_imp,ci),
       aes(x=air_temp_imp,y=o3)) +
  geom_point(shape=21,color="black",fill="green") + 
  stat_function(fun=function(x)params[1]+params[2]*x+params[3]*x^2,color="red") + 
  geom_ribbon(aes(ymin=lwr,ymax=upr),fill="blue",alpha=.45)

save(fit,file="data/beta.RData")

# decomposition ----
o3_df <- monthly(kc,"o3") %>%
  ungroup() %>% 
  select(year,month,o3)

start <- head(o3_df,n=1) 
end <- tail(o3_df,n=1)  

o3_dcmp <- o3_df %>% 
  ts(start=c(start$year,start$month),
     end=c(end$year,end$month),
     frequency=12) %>% 
  decompose()

season <- o3_dcmp$season %>% head(n=n_o3)

ggplot(data.frame(month=1:n_o3,
                  y=o3),
       aes(x=month,y=o3)) +
  geom_line()

# regression imputation ----
