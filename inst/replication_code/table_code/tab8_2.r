## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(dplyr)
library(psre)
library(stargazer)

## load data from psre package
data(wvs)

## listwise delete on gini
wvs <- wvs %>% 
  filter(!is.na(gini_disp))

## estimate raw polynomial model
polymod <- lm(moral ~ poly(gini_disp, 2, raw=TRUE) + I(pct_high_rel_imp^3), data=wvs)

## center gini variable
wvs1 <- wvs %>% mutate(gini_disp = gini_disp - mean(gini_disp, na.rm=TRUE))

## estimate centered polynomial model
cenpolymod <- lm(moral ~ poly(gini_disp, 2, raw=TRUE) + I(pct_high_rel_imp^3), data=wvs1)

## estimate orthogonal polynomial model
orthpolymod <- lm(moral ~ poly(gini_disp, 2) + I(pct_high_rel_imp^3), data=wvs)

## print models
stargazer(polymod, cenpolymod, orthpolymod, digits=2, type="text")

