# packages 
library(evir)
library(ismev)
library(extRemes)
library(tidyverse)

# reference ----
url <- "https://qiita.com/hrkz_szk/items/2c966aab9342f61a5b59"

# get data ----
data(bmw) # from "evir"

# data handlings ----
r <- 2
bmwLoss <- -bmw * 100
years <- attr(bmwLoss,"times") %>% format("%Y")
bmwOrder <- data.frame(years=str_sub(years,start=-2),y=bmwLoss) %>%
  group_by(years) %>% 
  slice_max(order_by=y,n=r) %>%
  mutate(r=dense_rank(desc(y))) #  

# data plot ----
g <- ggplot(data.frame(t=attr(bmw,"times") %>% format("%Y/%m/%d") %>% as.Date(), y=bmw),
            aes(x=t,y=y)) +
  geom_line()

z <- ggplot(bmwOrder,
            aes(x=years,y=y,fill=factor(r))) + 
  geom_point(shape=21,color="black") + 
  guides(fill=guide_legend(title="r"))

plot(g)
plot(z)

# the r largest order model: ismev ====
obj <- bmwOrder %>% # MUST CREATE obj (data.frame) for rlarg.fit() !!! 
  pivot_wider(names_from=r,values_from=y) %>% 
  ungroup() %>% 
  select(-years) %>% 
  as.data.frame()

fit.rlarg <- rlarg.fit(obj) 

# model diagnostics ----
rlarg.diag(fit.rlarg)

# CI: 
par.rlarg <- fit.rlarg$mle # parameter estimates *MLE
lower <- par.rlarg - 1.96*fit.rlarg$se
upper <- par.rlarg + 1.96*fit.rlarg$se
ci <- data.frame(lower=lower,
                 upper=upper,
                 row.names=c("loc","scale","shape")) 
