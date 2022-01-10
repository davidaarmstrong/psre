## Note, table 11.4 is hypothetical and not made from 
## any real data.  As such, the next statistical table
## after 11.3 is 11.5.

## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages

library(tidyverse)
library(ordinal)
library(psre)
library(stargazer)
library(car)
library(DAMisc)

## load data from psre package
data(repress)
## replace pr = NA if pr < 0
repress$pr <- ifelse(repress$pr < 0, NA, repress$pr)
## rescale pr 
repress$pr <- repress$pr/100

## generate variables and convert to factor
repress <- repress %>% mutate(log_gdp = log(rgdpe), 
                              logpop = log(pop),
                              pts_fac = as.factor(pts_s), 
                              cwar = as.factor(cwar), 
                              iwar = as.factor(iwar)) %>% 
  dplyr::select(pr, cwar, iwar, rgdpe, pop, log_gdp, logpop, pts_s, pts_fac) %>% 
  na.omit()

## make basis functions
BL <- function(x,c=.34)ifelse(x < c, c-x, 0)
BR <- function(x,c=.34)ifelse(x > c, x-c, 0)

## linear model of pts
lpwl <- lm(pts_s ~ BL(pr, .34) + BR(pr, .34) + cwar + iwar + log(rgdpe) + log(pop), data=repress)

## ordered logit model of pts
opwl <- clm(pts_fac ~ BL(pr, .34) + BR(pr, .34) + cwar + iwar + log(rgdpe) + log(pop), data=repress)

## print models
stargazer(lpwl, opwl, digits=3, type="text")


## Test of Equal Spacing of Cut Points. 
linearHypothesis(opwl, c("2|3-1|2 =3|4-2|3", "3|4-2|3 =4|5-3|4"))

## (e)PRE statistics
repress$bl <- BL(repress$pr, .34)
repress$br <- BR(repress$pr, .34)
opwl2 <- MASS::polr(pts_fac ~ bl + br + cwar + iwar + log(rgdpe) + log(pop), data=repress)
pre(opwl2, sim=TRUE)





