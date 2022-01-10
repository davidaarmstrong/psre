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

## ordered logit model of pts
opwl <- clm(pts_fac ~ BL(pr, .34) + BR(pr, .34) + cwar + iwar + log(rgdpe) + log(pop), data=repress)

## make hypothetical observation where civil war 
## changes from 0 to 1. 
fake <- data.frame(
  rgdpe=95000, 
  pr = .25, 
  pop = 9.5, 
  iwar = factor(1, 1:2, c("0", "1")), 
  cwar = as.factor(c(0,1)), 
  pts_s = factor(1, 1:5)
)

## get predicted probabilities (cumulative probabilities)
## for hypothetical data
p <- predict(opwl, newdata=fake, type="cum.prob")$cprob1

## get predicted probabilities 
## for hypothetical data
p2 <- predict(opwl, newdata=fake, type="prob")$fit

## Collect results
tab11_6 <- round(cbind(p2[1,], p[1,], p2[2,], p[2,]), 3)
colnames(tab11_6) <- c("prob_~CW", "cprob_~CW", "prob_CW", "cprob_CW")

tab11_6

