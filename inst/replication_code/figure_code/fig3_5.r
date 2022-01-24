## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages

library(tidyverse)
library(psre)
library(sn)
## you'll need the fGarch package, but we don't need to 
## load it using the library() function. 

## set the random number generating seed so you can produce
## exactly the same graphs as in the text
set.seed(1234)
## draw four different sets of values whose distributions
## have the desired properties. 

## Normal
x1 <- rnorm(250)
## Bi-modal
x2 <- c(rnorm(125, -3), rnorm( 125, 3))
## Leptokurtic
x3 <- fGarch::rged(250,  mean=0, sd=sqrt(2), nu=1)
## Platykurtic
x4 <- fGarch::rged(250,  mean=0, sd=sqrt(2), nu=10)

## Negatively Skewed
x5 <- rsn(250,0,1,-100)
## Positively Skewed
x6 <- rsn(250, 0, 1, 100)

## collect all data into a single data frame
dat <- data.frame(
  x1 = x1, 
  x2 = x2, 
  x3 = x3, 
  x4 = x4, 
  x5 = x5, 
  x6 = x6
)

## The qqPoints function from the psre package produces a data frame
## that has both theoretical and observed quantiles along with 
## lower and upper confidence bounds (in lwr and upr, respectively).  
## The data frame also has an "ab" attribute that allows you to make
## the line that runs diagonally through the plot.  We create two 
## values 'l' and 'u', and the line is drawn from (min(theo), l) to
## (max(theo), u).  

## Panel (a): Normal
qqdf <- qqPoints(dat$x1)
a <- attr(qqdf, "ab")[1]
b <- attr(qqdf, "ab")[2]
l <- min(qqdf$theo) * b + a
u <- max(qqdf$theo) * b + a

ggplot(qqdf, aes(x=theo, y=x)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15) + 
  geom_segment(aes(x=min(qqdf$theo), xend=max(qqdf$theo), y = l, yend=u)) + 
  geom_point(shape=1) + 
  theme_classic() + 
  labs(x="Theoretical Quantiles", 
       y="Observed Quantiles")
# ggssave("output/f3_5a.png", height=4.5, width=4.5, units="in", dpi=300)


## Panel (b): Bi-modal
qqdf <- qqPoints(dat$x2)
a <- attr(qqdf, "ab")[1]
b <- attr(qqdf, "ab")[2]
l <- min(qqdf$theo) * b + a
u <- max(qqdf$theo) * b + a


qqdf <- qqdf %>% mutate(outside = factor(ifelse(x < lwr | x > upr, 2, 1)))
ggplot(qqdf, aes(x=theo, y=x)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15) + 
  geom_segment(aes(x=min(qqdf$theo), xend=max(qqdf$theo), y = l, yend=u)) + 
  geom_point(shape=1) + 
  theme_classic() + 
  labs(x="Theoretical Quantiles", 
       y="Observed Quantiles")
# ggssave("output/f3_5b.png", height=4.5, width=4.5, units="in", dpi=300)

## Panel (c): Leptokurtic (Light Tails)

qqdf <- qqPoints(dat$x3)
a <- attr(qqdf, "ab")[1]
b <- attr(qqdf, "ab")[2]
l <- min(qqdf$theo) * b + a
u <- max(qqdf$theo) * b + a

qqdf <- qqdf %>% mutate(outside = factor(ifelse(x < lwr | x > upr, 2, 1)))
ggplot(qqdf, aes(x=theo, y=x)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15) + 
  geom_segment(aes(x=min(qqdf$theo), xend=max(qqdf$theo), y = l, yend=u)) + 
  geom_point(shape=1) + 
  theme_classic() + 
  labs(x="Theoretical Quantiles", 
       y="Observed Quantiles")
# ggssave("output/f3_5c.png", height=4.5, width=4.5, units="in", dpi=300)

## Panel (d): Platykurtic (Heavy Tails)

qqdf <- qqPoints(dat$x4)
a <- attr(qqdf, "ab")[1]
b <- attr(qqdf, "ab")[2]
l <- min(qqdf$theo) * b + a
u <- max(qqdf$theo) * b + a

qqdf <- qqdf %>% mutate(outside = factor(ifelse(x < lwr | x > upr, 2, 1)))
ggplot(qqdf, aes(x=theo, y=x)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15) + 
  geom_segment(aes(x=min(qqdf$theo), xend=max(qqdf$theo), y = l, yend=u)) + 
  geom_point(shape=1) + 
  theme_classic() + 
  labs(x="Theoretical Quantiles", 
       y="Observed Quantiles")
# ggssave("output/f3_5d.png", height=4.5, width=4.5, units="in", dpi=300)

## Panel (e): Negative Skew

qqdf <- qqPoints(dat$x5)
a <- attr(qqdf, "ab")[1]
b <- attr(qqdf, "ab")[2]
l <- min(qqdf$theo) * b + a
u <- max(qqdf$theo) * b + a

qqdf <- qqdf %>% mutate(outside = factor(ifelse(x < lwr | x > upr, 2, 1)))
ggplot(qqdf, aes(x=theo, y=x)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15) + 
  geom_segment(aes(x=min(qqdf$theo), xend=max(qqdf$theo), y = l, yend=u)) + 
  geom_point(shape=1) + 
  theme_classic() + 
  labs(x="Theoretical Quantiles", 
       y="Observed Quantiles")
# ggssave("output/f3_5e.png", height=4.5, width=4.5, units="in", dpi=300)

## Panel (f): Positive Skew

qqdf <- qqPoints(dat$x6)
a <- attr(qqdf, "ab")[1]
b <- attr(qqdf, "ab")[2]
l <- min(qqdf$theo) * b + a
u <- max(qqdf$theo) * b + a

qqdf <- qqdf %>% mutate(outside = factor(ifelse(x < lwr | x > upr, 2, 1)))
ggplot(qqdf, aes(x=theo, y=x)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15) + 
  geom_segment(aes(x=min(qqdf$theo), xend=max(qqdf$theo), y = l, yend=u)) + 
  geom_point(shape=1) + 
  theme_classic() + 
  labs(x="Theoretical Quantiles", 
       y="Observed Quantiles")
# ggssave("output/f3_5f.png", height=4.5, width=4.5, units="in", dpi=300)
