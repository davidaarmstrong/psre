## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(ggrepel)
library(car)
# library(MASS)
# library(L1pack)
## you need the MASS and L1pack packages, but we will 
## not load them, we will just call functions from 
## their namespaces. 

## load data from psre package
data(wvs)

## filter to only waves 2 and 3 of the data. 
## turn democrat into a factor with the appropriate labels
## keep only the first instance where the country appears in the data
wvs0 <- wvs %>% 
  filter(wave %in% c(3,2)) %>% 
  mutate(democrat= factor(democrat, levels=1:2, 
                          labels=c("New Democracy", "Established Democracy"))) %>%
  group_by(country) %>% 
  arrange(wave) %>% 
  slice_head(n=1) 

## Run the linear model 
lmod <- lm(secpay ~ gini_disp + democrat, data=wvs0)

## Estimate the least absolute deviations regression model and 
## save the residuals
e1_lad <- L1pack::lad(secpay ~ gini_disp + democrat, data=wvs0)$residuals

## Estimate a robust linear regression using an M estimator
## and save the residuals. 
e1_m <- MASS::rlm(secpay ~ gini_disp + democrat, data=wvs0, 
                  method="M")$residuals

## Estimate a robust linear regression using an M-M estimator
## and save the residuals. 
e1_mm <- MASS::rlm(secpay ~ gini_disp + democrat, data=wvs0, 
                   method="MM")$residuals

## Collect all of the residuals in a single data frame. 
e1dat <- data.frame(OLS = lmod$residuals, 
                    LAD = e1_lad, 
                    M = e1_m, 
                    MM = e1_mm)

## A. Linear model RR Plot
rrPlot(OLS ~ LAD + M + MM, data=e1dat)
# ggssave("output/f7_6a.png", height=3, width=8, dpi=300)


## proceed as above, but using the cubic polynomial in gini
pmod <- lm(secpay ~ poly(gini_disp,3) + democrat, data=wvs0)
e2_lad <- L1pack::lad(secpay ~ poly(gini_disp,3) + democrat, data=wvs0)$residuals
e2_m <- MASS::rlm(secpay ~ poly(gini_disp,3) + democrat, data=wvs0, 
                  method="M")$residuals
e2_mm <- MASS::rlm(secpay ~ poly(gini_disp,3) + democrat, data=wvs0, 
                   method="MM")$residuals


e2dat <- data.frame(OLS = pmod$residuals, 
                    LAD = e2_lad, 
                    M = e2_m, 
                    MM = e2_mm)

rrPlot(OLS ~ LAD + M + MM, data=e2dat)
# ggssave("output/f7_6b.png", height=3, width=8, dpi=300)

## Hypothesis Tests for Slopes and Intercepts: 

m1 <- lm(OLS ~ LAD, data=e1dat)
m2 <- lm(OLS ~ M, data=e1dat)
m3 <- lm(OLS ~ MM, data=e1dat)

linearHypothesis(m1, c("(Intercept)  = 0", "LAD = 1"))
linearHypothesis(m2, c("(Intercept)  = 0", "M = 1"))
linearHypothesis(m3, c("(Intercept)  = 0", "MM = 1"))

m1 <- lm(OLS ~ LAD, data=e2dat)
m2 <- lm(OLS ~ M, data=e2dat)
m3 <- lm(OLS ~ MM, data=e2dat)

linearHypothesis(m1, c("(Intercept)  = 0", "LAD = 1"))
linearHypothesis(m2, c("(Intercept)  = 0", "M = 1"))
linearHypothesis(m3, c("(Intercept)  = 0", "MM = 1"))




