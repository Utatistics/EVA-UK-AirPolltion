# packages 
library(magick)
library(extRemes)
library(tidyverse)
library(gridExtra)

# reference ----
url <- "https://qiita.com/cotton-gluon/items/bd1a12e22cf4c2d8b805"

# Gumbel family ====
# Exponential ----
rgumb <- function(m,n){
  sample <- vector("numeric",m) # generate empty vector 
  an <- 1
  bn <- log(n)
  for (i in 1:m){
    x <- rexp(n,rate=1)
    z <- (max(x) - bn) / an 
    sample[i] <- z
  }
  return(sample)
}
qq <- qevd(c(1/m, 1 - 1/(10*m)),loc=0,scale=1,shape=0) 
gumb.plot <- function(m,n){
  z <- rgumb(m,n)
  g <- ggplot(data.frame(zn=z),aes(x=zn)) + 
    geom_histogram(aes(y=..density..),color="black",fill="orange",bins=50) + 
    stat_function(data=data.frame(q=seq(min(z),max(z),len=100)),
                  aes(x=q),
                  fun=devd, 
                  args=list(loc=0,scale=1,shape=0), # Gumbel family 
                  linetype="dashed",color="blue",
                  size=.75) +
    ggtitle(paste0("n = ",n)) + 
    theme(plot.title=element_text(size=16,hjust=0.5)) + 
    labs(x="") +
    xlim(qq[1],qq[2]) +
    ylim(0,.4)
  
  return(g)
}
gumb.plot2 <- function(m,n){
  z <- rgumb(m,n)
  g <- ggplot(data.frame(zn=z),aes(x=zn)) + 
    geom_density(color="black",fill="orange",alpha=.65) + 
    stat_function(data=data.frame(q=seq(min(z),max(z),len=100)),
                  aes(x=q),
                  fun=devd, 
                  args=list(loc=0,scale=1,shape=0), # Gumbel family 
                  linetype="dashed",color="blue",
                  size=.75) +
    ggtitle(paste0("n = ",n)) + 
    theme(plot.title=element_text(size=16,hjust=0.5)) + 
    labs(x="") +
    xlim(qq[1],qq[2]) +
    ylim(0,.4)
  
  return(g)
}

# animated gif
img=image_graph()
par()
m <- 5e3
for (n in c(3,5,10,25,50,100,500,1000)){
  g <- gumb.plot(m,n)
  plot(g)
}
dev.off()
animation <- image_animate(img,fps=1,loop=0)
image_write(animation,path="output/gumbel.gif",format="gif")

# animated gif2
img=image_graph()
par()
m <- 5e3
for (n in c(5,10,25,50,100,500,1000)){
  g <- gumb.plot2(m,n)
  plot(g)
}
dev.off()
animation <- image_animate(img,fps=2,loop=0)
image_write(animation,path="output/gumbel2.gif",format="gif")

# Gaussian ----
# Fre´chet family ====
# Cauchy-Lorentz ----
# Zipf *Pareto ----
# Weibull family ====
# Uniform ----
# negative Weibull ----

