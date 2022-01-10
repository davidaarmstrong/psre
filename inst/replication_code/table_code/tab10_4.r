## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(rio)
library(splines)
library(clarkeTest)
library(stargazer)
library(boot)

## load data from psre package
data(gss)

## rescale sei01 into the range (0,1)
## create sex factor
## recode education to 6 if it is 
## currently less than 6. 
## select required variables and listwise delete
gss <- gss %>% 
  mutate(sei01 = sei10/100, 
         sex = factorize(sex),
         educ = case_when(
           educ < 6 ~ 6, 
           TRUE ~ educ)) %>% 
  dplyr::select(childs, age, sei01, sex, educ) %>% 
  na.omit()

## estimate glm that is additive and linear (in the link) 
## for age
moda <- glm(childs ~ age + sei01 + sex + educ, 
            data=gss, family=poisson)

## initialize list to hold alternative models
mlist <- list()

## get data from moda
tmp <- model.frame(moda) 

## raw data model 
mlist[[1]] <- glm(childs ~ age + sei01 + sex + educ,
                  data=gss, family=poisson)

## quadratic polynomial in age
mlist[[2]] <- glm(childs ~ poly(age, 2) + sei01 + sex + educ,
                  data=gss, family=poisson)

## cubic polynomial in age
mlist[[3]] <- glm(childs ~ poly(age, 3) + sei01 + sex + educ,
                  data=gss, family=poisson)

## quartic polynomial in age
mlist[[4]] <- glm(childs ~ poly(age, 4) + sei01 + sex + educ,
                  data=gss, family=poisson)

## quadratic b-spline with one knot in age
mlist[[5]] <- glm(childs ~ bs(age, degree=2, knots=41.5) + sei01 + sex + educ,
                  data=gss, family=poisson)

## quadratic b-spline with two knots in age
mlist[[6]] <- glm(childs ~ bs(age, degree=2, knots=c(43.5, 80.5)) + sei01 + sex + educ,
                  data=gss, family=poisson)

## cubic b-spline with one knot in age
mlist[[7]] <- glm(childs ~ bs(age, knots=64.5) + sei01 + sex + educ,
                  data=gss, family=poisson)

## calculate fit statistics for all models
## log-likelihood
LL <- sapply(mlist, logLik)
## aic and bic
aics <- sapply(mlist, AIC)
bics <- sapply(mlist, BIC)
## deltas for aic and bic
deltas <- aics-min(aics)
deltab <- bics-min(bics)

## clarke tests against the B-spline (2,2) model
ct <- list()
ct[[1]] <- clarke_test(mlist[[6]], mlist[[1]]) # 6 preferred
ct[[2]] <- clarke_test(mlist[[6]], mlist[[2]]) # 6 preferred
ct[[3]] <- clarke_test(mlist[[6]], mlist[[3]]) # 6 preferred
ct[[4]] <- clarke_test(mlist[[6]], mlist[[4]]) # N
ct[[5]] <- clarke_test(mlist[[6]], mlist[[5]]) # 5 preferred
ct[[6]] <- clarke_test(mlist[[6]], mlist[[7]]) # N

## calculate p-values for tests
calcp <- function(x){
  d <- min(x$stat, x$nobs-x$stat)
  p <- 2*pbinom(d, x$nobs, .5)
  p
}
pvals <- sapply(ct, calcp)

## calculate cross-validation error.  These results
## are slightly different from the book due to the 
## randomness of the cross-validation splitting.  
## we will use this seed in future editions. 
set.seed(207)
cvres <- sapply(1:7, function(i)replicate(100, cv.glm(tmp, mlist[[i]], K=10)$delta[2]))
pct_cv <- apply(cvres[,-6], 2, function(x)mean(x > cvres[,6]))



tab10_4 <- tibble(model = 1:7, 
                  specification = c("Linear", "Polynomial(2)", "Polynomial(3)", 
                                    "Polynomial(4)", "BS(2,1)", "BS(2,2)", 
                                    "BS(3,1)"), 
                  LL = LL, 
                  delta_AIC = deltas, 
                  delta_BIC = deltab, 
                  p_vs_M6 = round(c(pvals[1:5], NA, pvals[6]), 3), 
                  CV_vs_M6  = c(pct_cv[1:5], NA, pct_cv[6])*100)


tab10_4

