## There are slight discrepancies here between the 
## delta AIC and BIC values presented in the table
## and between the coefficients for model 8h calculated
## here and those presented in the table.  

## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(stargazer)

## load data from psre package
data(repress)

## make left and right piecewise linear basis functions
BL <- function(x,c=.34)ifelse(x < c, c-x, 0)
BR <- function(x,c=.34)ifelse(x > c, x-c, 0)

## remove pr values less than zero
repress$pr <- ifelse(repress$pr < 0, NA, repress$pr)
## rescale pr
repress$pr <- repress$pr/100
## make log_gdp and logpop  variables
repress <- repress %>% mutate(log_gdp = log(rgdpe), 
                              logpop = log(pop)) %>% 
  select(pr, cwar, iwar, rgdpe, pop, log_gdp, logpop, pts_s) %>% 
  ## listwise delete
  na.omit()

## linear model 
mlin <- lm(pts_s ~ pr + cwar + iwar + log(rgdpe) + log(pop), data=repress)
## quadratic polynomial model
mpol <- lm(pts_s ~ pr + I(pr^2) + cwar + iwar + log(rgdpe) + log(pop), data=repress)

## How we arrived at the appropriate knot location for the piecewise linear spline
s <- seq(.06, .39, length=100)
l <- lapply(s, function(k)lm(pts_s ~ BL(pr, k) + BR(pr, k) + cwar + iwar + log(rgdpe) + log(pop), data=repress))
knot_aic <- lapply(l, AIC)
## optimal knot location 
s[which.min(knot_aic)]

## piecewise linear model
mpwl <- lm(pts_s ~ BL(pr, .353) + BR(pr, .353) + cwar + iwar + log(rgdpe) + log(pop), data=repress)

## make fit statistics
a <- AIC(mlin, mpol, mpwl)[,2]
delta_a <- round(a-min(a), 1)
b <- BIC(mlin, mpol, mpwl)[,2]
delta_b <- round(b-min(b), 1)
stats <- tibble(
  Measure = c("Delta_AIC", "Delta_BIC"), 
  M8f = c(delta_a[1], delta_b[1]), 
  M8g = c(delta_a[2], delta_b[2]), 
  M8h = c(delta_a[3], delta_b[3])
)

stargazer(mlin, mpol, mpwl, digits=2, type="text")
stats




