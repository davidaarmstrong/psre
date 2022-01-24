## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(ordinal)
library(psre)
library(ggridges)

## load data from psre package
data(repress)

## replace pr as NA if pr < 0
repress$pr <- ifelse(repress$pr < 0, NA, repress$pr)
## rescale pr to (0,1)
repress$pr <- repress$pr/100

## create transformed variables and turn binary 
## variables into factors
repress <- repress %>% mutate(log_gdp = log(rgdpe), 
                              logpop = log(pop),
                              pts_fac = as.factor(pts_s), 
                              cwar = as.factor(cwar), 
                              iwar = as.factor(iwar)) %>% 
  dplyr::select(pr, cwar, iwar, rgdpe, pop, log_gdp, logpop, pts_s, pts_fac) %>% 
  na.omit()

## make piecewise linear basis functions
BL <- function(x,c=.34)ifelse(x < c, c-x, 0)
BR <- function(x,c=.34)ifelse(x > c, x-c, 0)

## ordered logit with piecewise linear spline for pr
opwl <- clm(pts_fac ~ BL(pr, .34) + BR(pr, .34) + cwar + iwar + log(rgdpe) + log(pop), data=repress)
## same ordered logit model, but using polr from MASS
opwlm <- MASS::polr(pts_fac ~ BL(pr, .34) + BR(pr, .34) + cwar + iwar + log(rgdpe) + log(pop), data=repress)
## linear model 
linpwl <- lm(pts_s ~ BL(pr, .34) + BR(pr, .34) + cwar + iwar + log(rgdpe) + log(pop), data=repress)

## make a sequence of values from 0 to .4, the
## range of pr
s <- seq(0,.4, by=.01)

## initialize two containers for results
fit1 <- fit2 <- NULL
## loop over the hypothetical values of pr
for(i in 1:length(s)){
  ## get the data to estimate the model
  tmp <- get_all_vars(formula(opwlm), repress) %>% na.omit()
  ## set pr to the ith value of s
  tmp$pr <- s[i]
  ## generate expected value for this particular 
  ## value of s
  e1 <- predict(opwlm, newdata=tmp, type="prob")
  e1 <- c(e1 %*% 1:5)
  ## get predicted probabilities from the linear
  ## model for this value of s
  e2 <- predict(linpwl, newdata=tmp)
  ## save the results
  fit1 <- rbind(fit1, e1)
  fit2 <- rbind(fit2, e2)
}

## calculate the difference between the 
## two different measures of fit
dif <- abs(fit1-fit2)


## make density data
dens_dat <- NULL
## calculate density for the differences for 
## each of the values of s
for(i in 1:41){
  dd <- density(dif[i,], n=100, from=0, to=.4)
  ## collect the data - scaling the maximum density
  ## estimate for each value of s to 1
  dens_dat <- bind_rows(dens_dat, 
                        data.frame(x=dd$x, 
                                   y=dd$y/max(dd$y), 
                                   s=s[i]))
}

## make plot 
ggplot(dens_dat, aes(x=x, y=as.factor(sprintf("%.2f", s)))) + 
  geom_ridgeline(aes(height=y*.95), size=0) + 
  theme_classic() + 
  labs(x="Absolute Difference in Effects", 
       y="Political Rights")
# ggssave("output/f11_12.png", height=6.5, width=4.5, units="in", dpi=300)

## or with geom_density_ridges

dens_dat <- data.frame(
  diff = c(dif), 
  s = rep(as.factor(sprintf("%.2f", s)), ncol(dif)), 
  obs = rep(1:ncol(dif), each=length(s))
)

ggplot(dens_dat, aes(x=diff, y=s)) + 
  geom_density_ridges(scale=.9, col="transparent") + 
  theme_classic() + 
  labs(x="Absolute Difference in Effects", 
       y="Political Rights")
