## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages

library(tidyverse)
library(car)
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


## Make a temporary dataset that keeps x2 only and renames it as raw. 
tmp <- dat %>% select(x2) %>% setNames("raw")
## Calculate appropriate lambda for the Box-Cox transformation 
p1 <- powerTransform((dat$x2-min(dat$x2) + .1) ~ 1, family="bcPower")
## Add a variable called 'nl' to our data frame that contains the 
## optimal Box-Cox transformation.  In this case, we transformed the 
## variable so that its minimum value is .1
tmp <- tmp %>% mutate(nl = bcPower((raw - min(raw) + .1), p1$roundlam))
## Add a variable called 'ihs' that has the IHS transform in it. 
tmp <- tmp %>% mutate(ihs = asinh(raw))
## Find the appropriate transformation parameter for the Y-J transformation
p1y <- powerTransform(tmp$raw ~ 1, family="yjPower")
## Add a variable called 'yj' that has the Y-J transform in it. 
tmp <- tmp %>% mutate(yj = yjPower(raw, p1y$roundlam))
## Find the appropriate lambda and gamma parameters for the BCN transformation
p1n <- powerTransform(tmp$raw ~ 1, family="bcnPower")
## Add a variable called 'bcn' that has the BCN transform in it. 
tmp <- tmp %>% mutate(bcn = bcnPower(raw, lambda = p1n$roundlam, gamma=p1n$gamma)) 
## pivot the data to longer making a variable that identifies the 
## type of transformation ('trans') and the values of the transformation
## ('vals').
tmp1 <- tmp %>% 
  pivot_longer(everything(), names_to="trans", values_to="vals") %>% 
  ## make trans into a factor
  mutate(trans = factor(trans, 
                        levels=c("raw", "nl", "ihs", "yj", "bcn"), 
                        labels=c("Raw", "Box-Cox", "IHS", "Yeo-Johnson", "Box-Cox N")), 
  ## make a variable called 'dist' that identifies the distribution of x. 
         dist = "Bi-modal")


## We follow the same steps as above for the 
## other non-normal variables in our data frame


p1 <- powerTransform((dat$x3-min(dat$x3) + .1) ~ 1, family="bcPower")
tmp <- dat %>% select(x3) %>% setNames("raw")
tmp <- tmp %>% mutate(nl = bcPower((raw - min(raw) + .1), p1$roundlam))
tmp <- tmp %>% mutate(ihs = asinh(raw))
p1y <- powerTransform(tmp$raw ~ 1, family="yjPower")
tmp <- tmp %>% mutate(yj = yjPower(raw, p1y$roundlam))
p1n <- powerTransform(tmp$raw ~ 1, family="bcnPower")
tmp <- tmp %>% mutate(bcn = bcnPower(raw, lambda = p1n$roundlam, gamma=p1n$gamma)) 
tmp2 <- tmp %>% 
  pivot_longer(everything(), names_to="trans", values_to="vals") %>% 
  mutate(trans = factor(trans, 
                        levels=c("raw", "nl", "ihs", "yj", "bcn"), 
                        labels=c("Raw", "Box-Cox", "IHS", "Yeo-Johnson", "Box-Cox N")), 
         dist = "Leptokurtic")

p1 <- powerTransform((dat$x4-min(dat$x4) + .1) ~ 1, family="bcPower")
tmp <- dat %>% select(x4) %>% setNames("raw")
tmp <- tmp %>% mutate(nl = bcPower((raw - min(raw) + .1), p1$roundlam))
tmp <- tmp %>% mutate(ihs = asinh(raw))
p1y <- powerTransform(tmp$raw ~ 1, family="yjPower")
tmp <- tmp %>% mutate(yj = yjPower(raw, p1y$roundlam))
p1n <- powerTransform(tmp$raw ~ 1, family="bcnPower")
tmp <- tmp %>% mutate(bcn = bcnPower(raw, lambda = p1n$roundlam, gamma=p1n$gamma)) 
tmp3 <- tmp %>% 
  pivot_longer(everything(), names_to="trans", values_to="vals") %>% 
  mutate(trans = factor(trans, 
                        levels=c("raw", "nl", "ihs", "yj", "bcn"), 
                        labels=c("Raw", "Box-Cox", "IHS", "Yeo-Johnson", "Box-Cox N")), 
         dist = "Platykurtic")

p1 <- powerTransform((dat$x5-min(dat$x5) + .1) ~ 1, family="bcPower")
tmp <- dat %>% select(x5) %>% setNames("raw")
tmp <- tmp %>% mutate(nl = bcPower((raw - min(raw) + .1), p1$roundlam))
tmp <- tmp %>% mutate(ihs = asinh(raw))
p1y <- powerTransform(tmp$raw ~ 1, family="yjPower")
tmp <- tmp %>% mutate(yj = yjPower(raw, p1y$roundlam))
p1n <- powerTransform(tmp$raw ~ 1, family="bcnPower")
tmp <- tmp %>% mutate(bcn = bcnPower(raw, lambda = p1n$roundlam, gamma=p1n$gamma)) 
tmp4 <- tmp %>% 
  pivot_longer(everything(), names_to="trans", values_to="vals") %>% 
  mutate(trans = factor(trans, 
                        levels=c("raw", "nl", "ihs", "yj", "bcn"), 
                        labels=c("Raw", "Box-Cox", "IHS", "Yeo-Johnson", "Box-Cox N")), 
         dist = "Left-skew")

p1 <- powerTransform((dat$x6-min(dat$x6) + .1) ~ 1, family="bcPower")
tmp <- dat %>% select(x6) %>% setNames("raw")
tmp <- tmp %>% mutate(nl = bcPower((raw - min(raw) + .1), p1$roundlam))
tmp <- tmp %>% mutate(ihs = asinh(raw))
p1y <- powerTransform(tmp$raw ~ 1, family="yjPower")
tmp <- tmp %>% mutate(yj = yjPower(raw, p1y$roundlam))
p1n <- powerTransform(tmp$raw ~ 1, family="bcnPower")
tmp <- tmp %>% mutate(bcn = bcnPower(raw, lambda = p1n$roundlam, gamma=p1n$gamma)) 
tmp5 <- tmp %>% 
  pivot_longer(everything(), names_to="trans", values_to="vals") %>% 
  mutate(trans = factor(trans, 
                        levels=c("raw", "nl", "ihs", "yj", "bcn"), 
                        labels=c("Raw", "Box-Cox", "IHS", "Yeo-Johnson", "Box-Cox N")), 
         dist = "Right-skew")

## put all data together from each distribution
tmp_all <- bind_rows(tmp1, tmp2, tmp3, tmp4, tmp5)

## calculate the density estimates for each transformation-distribution pair
tmp_dens <- tmp_all %>% group_by(trans, dist) %>% 
  summarise(normBand(vals))

## Make the plot
ggplot(tmp_dens, aes(x=eval.points)) + 
  geom_ribbon(aes(ymin = lwr, ymax=upr), alpha=.25, fill="gray50") + 
  geom_ribbon(aes(ymin = lwd_od, ymax = upr_od), col="transparent", alpha=.5) + 
  geom_line(aes(y=obsden), col="black") + 
  facet_wrap(trans ~ dist, scales="free") + 
  theme_bw() + 
  theme(panel.grid=element_blank()) + 
  labs(x="Values of X", y="Density")
# ggssave("output/f3_10.png", height=8, width=8, units="in", dpi=150)
