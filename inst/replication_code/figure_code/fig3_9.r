## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages

library(tidyverse)
library(car)

## create data that will be used to demonstrate the
## transformations. 
x <- seq(-5, 5, length=100)

## IHS is a function that calculates the inverse hyperbolic sine 
## transformation. 
IHS <- function(x)log(x+sqrt(x^2+1))

## Panel (a): IHS
ggplot(data.frame(x=x), aes(x=x, y=IHS(x))) + 
  geom_line() + 
  theme_classic() + 
  labs(x="x", y="IHS(x)")
# ggssave("output/f3_9a.png", height=4.5, width=4.5, units="in", dpi=300)


d1b <- data.frame(
  x=x, 
  y=yjPower(x, lambda=-1), 
  lambda = factor(1, levels=1:3, labels=c("-1", "0", ".5"))
)
d2b <- data.frame(
  x=x, 
  y=yjPower(x, lambda=0), 
  lambda = factor(2, levels=1:3, labels=c("-1", "0", ".5"))
)

d3b <- data.frame(
  x=x, 
  y=yjPower(x, lambda=.5), 
  lambda = factor(3, levels=1:3, labels=c("-1", "0", ".5"))
)

db <- rbind(d1b, d2b, d3b)



## Panel (b): Yeo-Johnson
ggplot(db, aes(x=x, y=y, linetype=lambda)) + 
  geom_line()+ 
  theme_classic() + 
  theme(legend.position = c(.75, .25)) + 
  labs(x="x", y="YJ(x)", linetype="Lambda")
# ggssave("output/f3_9b.png", height=4.5, width=4.5, units="in", dpi=300)


d1 <- data.frame(
  x= x, y = bcnPower(x, lambda=-1, gamma=.5), 
  lambda = factor(1, levels=1:3, labels=c("-1", " 0", ".5")),
  gamma = factor(1, levels=1:2, labels=c("Gamma = .5", "Gamma = 2"))
)
d2 <- data.frame(
  x= x, y = bcnPower(x, lambda=.5, gamma=.5), 
  lambda = factor(2, levels=1:3, labels=c("-1", " 0", ".5")),
  gamma = factor(1, levels=1:2, labels=c("Gamma = .5", "Gamma = 2"))
)
d3 <- data.frame(
  x= x, y = bcnPower(x, lambda=0, gamma=.5), 
  lambda = factor(3, levels=1:3, labels=c("-1", " 0", ".5")),
  gamma = factor(1, levels=1:2, labels=c("Gamma = .5", "Gamma = 2"))
)
d4 <- data.frame(
  x= x, y = bcnPower(x, lambda=-1, gamma=2), 
  lambda = factor(1, levels=1:3, labels=c("-1", " 0", ".5")),
  gamma = factor(2, levels=1:2, labels=c("Gamma = .5", "Gamma = 2"))
)
d5 <- data.frame(
  x= x, y = bcnPower(x, lambda=.5, gamma=2), 
  lambda = factor(2, levels=1:3, labels=c("-1", " 0", ".5")),
  gamma = factor(2, levels=1:2, labels=c("Gamma = .5", "Gamma = 2"))
)
d6 <- data.frame(
  x= x, y = bcnPower(x, lambda=0, gamma=2), 
  lambda = factor(3, levels=1:3, labels=c("-1", " 0", ".5")),
  gamma = factor(2, levels=1:2, labels=c("Gamma = .5", "Gamma = 2"))
)

bcn_dat1 <- bind_rows(d1, d2, d3)
bcn_dat2 <- bind_rows(d4, d5, d6)

## Panel (c): BCN (gamma=.5)
ggplot(bcn_dat1, aes(x=x, y=y, linetype=lambda)) +
  geom_line() + 
  theme_classic() + 
  theme(panel.grid=element_blank(), 
        legend.position = c(.75, .25),
        aspect.ratio=1) + 
  labs(x="x", y="BCn(x)", linetype="Lambda")
# ggssave("output/f3_9c.png", height=4.5, width=4.5, units="in", dpi=300)

ggplot(bcn_dat2, aes(x=x, y=y, linetype=lambda)) +
  geom_line() + 
  theme_classic() + 
  theme(panel.grid=element_blank(), 
        legend.position = c(.75, .25),
        aspect.ratio=1) + 
  labs(x="x", y="BCn(x)", linetype="Lambda")
# ggssave("output/f3_9d.png", height=4.5, width=4.5, units="in", dpi=300)
