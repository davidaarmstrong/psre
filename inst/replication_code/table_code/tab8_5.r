## There are some discrepancies between this table and the 
## one in the book.  The cross-validation numbers are off
## as is the result indicating that the GAM (SS) and the 
## PWL model are not distinguishable according to the 
## clarke test - the PWL model is significantly better.  


## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(gamlss)
library(splines)
library(rsample)
library(clarkeTest)

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
  dplyr::select(pr, cwar, iwar, rgdpe, pop, log_gdp, logpop, pts_s) %>% 
  ## listwise delete
  na.omit()

## linear additive model
mlin <- lm(pts_s ~ pr + cwar + iwar + log(rgdpe) + log(pop), data=repress)
## polynomial in pr
mpol <- lm(pts_s ~ pr + I(pr^2) + cwar + iwar + log(rgdpe) + log(pop), data=repress)
## piecewise linear spline in pr
mpwl <- lm(pts_s ~ BL(pr, .34) + BR(pr, .34) + cwar + iwar + log(rgdpe) + log(pop), data=repress)
## cubic spline in pr (df=12)
mbs <- lm(pts_s ~ bs(pr, df = 12)+ cwar + iwar + log_gdp + logpop, data=repress)

## GAM with loess smoother for pr
g2 <- gamlss(pts_s ~ lo(~pr, span=.15) + cwar + iwar + log_gdp + logpop, data=repress)
## GAM with penalized spline smooth for pr
g1 <- gamlss(pts_s ~ pb(pr) + cwar + iwar + log_gdp + logpop, data=repress)


a <- AIC(mlin, mpol, mpwl, mbs, g1, g2)[,2]
b <- BIC(mlin, mpol, mpwl, mbs, g1, g2)[,2]

delta_a <- round(a-min(a), 1)
delta_b <- round(b-min(b), 1)

indivLogLiks.gamlss <- function(model){
  s2e <- exp(predict(model, "sigma")[1])
  yhat <- predict(model, "mu")
  ll <- dnorm(y, yhat, s2e, log=TRUE)
}
nparams.gamlss <- function(model){
  length(model$y)-model$df.residual
}

t1 <- clarke_test(mpwl, mlin)
t2 <- clarke_test(mpwl, mpol)
t3 <- clarke_test(mpwl, mbs)
t4 <- clarke_test(mpwl, g1)
t5 <- clarke_test(mpwl, g2)




## calculate cross-validation error for the models above
set.seed(519)
rs <- vfold_cv(repress, v=10, repeats=25)
rs$e1 <- NA
rs$e2 <- NA
rs$e3 <- NA
rs$e4 <- NA
rs$e5 <- NA
rs$e6 <- NA
for(i in 1:nrow(rs)){
  tmpt <- assessment(rs$splits[[i]])
  tmps <- analysis(rs$splits[[i]])
  m1 <- lm(pts_s ~ pr + cwar + iwar + log(rgdpe) + log(pop), data=tmps)
  m2 <- lm(pts_s ~ pr + I(pr^2) + cwar + iwar + log(rgdpe) + log(pop), data=tmps)
  m3 <- lm(pts_s ~ BL(pr, .34) + BR(pr, .34) + cwar + iwar + log(rgdpe) + log(pop), data=tmps)
  m4 <- lm(pts_s ~ bs(pr, df = 12)+ cwar + iwar + log_gdp + logpop, data=tmps)
  m5 <- gamlss(pts_s ~ lo(~pr, span=.15) + cwar + iwar + log_gdp + logpop, data=tmps)
  m6 <- gamlss(pts_s ~ pb(pr) + cwar + iwar + log_gdp + logpop, data=tmps)
  yhat1 <- predict(m1, newdata=tmpt)
  yhat2 <- predict(m2, newdata=tmpt)
  yhat3 <- predict(m3, newdata=tmpt)
  yhat4 <- predict(m4, newdata=tmpt)
  yhat5 <- predict(m5, newdata=tmpt)
  yhat6 <- predict(m6, newdata=tmpt)
  y <- tmpt$pts_s
  rs$e1[i] <- sum((y-yhat1)^2)
  rs$e2[i] <- sum((y-yhat2)^2)
  rs$e3[i] <- sum((y-yhat3)^2)
  rs$e4[i] <- sum((y-yhat4)^2)
  rs$e5[i] <- sum((y-yhat5)^2)
  rs$e6[i] <- sum((y-yhat6)^2)
}

## collate and aggregate cross-validation results
e <- rs %>% 
  group_by(id) %>% 
  summarise(across(e1:e6, sum)) %>% 
  ungroup %>% 
  summarise(across(e1:e6, mean))

## get p-values for clarke-test results
ct <- c(
  pbinom(t1$stat, t1$nobs, prob=.5), 
  pbinom(t2$stat, t2$nobs, prob=.5), 
  NA, 
  pbinom(t3$stat, t3$nobs, prob=.5), 
  pbinom(t4$stat, t4$nobs, prob=.5), 
  pbinom(t5$stat, t5$nobs, prob=.5))

## make table
tab8_5 <- rbind(
  delta_a, 
  delta_b, 
  c(as.matrix(e)),   
  ct
)
  
tab8_5



