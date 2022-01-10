## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(stargazer)

## load data from psre package
data(wvs)

## create the democratic history factor
## keep only the first instance of a country in either wave 2 or 3
wvs0 <- wvs %>% 
  filter(wave %in% c(3,2)) %>% 
  mutate(democrat= factor(democrat, levels=1:2, 
                          labels=c("New Democracy", "Established Democracy"))) %>%
  group_by(country) %>% 
  arrange(wave) %>% 
  slice_head(n=1) %>% 
  ## remove Czech Republic and Slovakia
  filter(!(country %in% c("Czech Rep.", "Slovakia")))

## estimate linear interaction model
imod <- lm(secpay ~ gini_disp*democrat,data=wvs0)
## cubic spline in gini
smod3 <- lm(secpay ~ tpb(gini_disp, degree=3, knot_loc=.35) + democrat, data=wvs0)
## third-degree polynomial in gini
pmod3 <- lm(secpay ~ poly(gini_disp, 3) + democrat, data=wvs0)
## second-degree polynomial in gini
pmod2 <- lm(secpay ~ poly(gini_disp, 2) + democrat, data=wvs0)
## linear additive model
lmod <- lm(secpay ~ gini_disp + democrat, data=wvs0)

## calculate AIC and BIC
a <- AIC(lmod, pmod2, pmod3, smod3, imod)
b <- BIC(lmod, pmod2, pmod3, smod3, imod)

## calculate AIC and BIC deltas
da <- round(a-min(a), 1)
db <- round(b-min(b), 1)

## get adjusted R-squared
ar2 <- c(summary(lmod)$adj.r.squared,
         summary(pmod2)$adj.r.squared,
         summary(pmod3)$adj.r.squared,
         summary(smod3)$adj.r.squared,
         summary(imod)$adj.r.squared)

## get model sums of squares
mss <- c(sum((fitted(lmod)-mean(wvs0$secpay, na.rm=TRUE))^2),
         sum((fitted(pmod2)-mean(wvs0$secpay, na.rm=TRUE))^2),
         sum((fitted(pmod3)-mean(wvs0$secpay, na.rm=TRUE))^2),
         sum((fitted(smod3)-mean(wvs0$secpay, na.rm=TRUE))^2),
         sum((fitted(imod)-mean(wvs0$secpay, na.rm=TRUE))^2))

## get residual sums of squares
rss <- c(sum(residuals(lmod)^2),
         sum(residuals(pmod2)^2),
         sum(residuals(pmod3)^2),
         sum(residuals(smod3)^2),
         sum(residuals(imod)^2))

## get residual degrees of freedom
rdf <- c(lmod$df.residual, 
         pmod2$df.residual, 
         pmod3$df.residual,
         smod3$df.residual,
         imod$df.residual)


## make table
tab7_3 <- tibble(
  model = c("(A) Linear", "(B) Quadratic Polynomial", 
            "(C) Cubic Polynomial", "(D) Cubic Spline (1 knot)", 
            "(E) Linear Interaction"), 
  SSRes = rss, 
  SSMod = mss, 
  DF =rdf, 
  AdjR2 = ar2, 
  DeltaAIC = da[,2], 
  DeltaBIC = db[,2])

## There is a discrepancy here between the calculations
## and the text in the table The SSMod for linear should
## be 0.05, .1 for the quadratic polynomial, 
## 0.12 for the cubic polynomial, .13 for the cubic spline and 
## 0.14 for the linear model. 
## The residual degrees of freedom are all off by one as well. 

## print table
tab7_3 %>% 
  mutate(across(-model, ~round(.x, 2)))

