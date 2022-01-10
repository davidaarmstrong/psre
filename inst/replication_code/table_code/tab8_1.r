## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(car)
library(psre)
library(stargazer)

## load data from psre package
data(wvs)

## Use the Box-Tidwell procedure to 
## choose the right transformation power
boxTidwell(moral ~ pct_high_rel_imp, ~secpay, data=wvs)
boxTidwell(moral ~ I(pct_high_rel_imp^3), ~secpay, data=wvs)
boxTidwell(moral ~ I(pct_high_rel_imp^4), ~secpay, data=wvs)

## estimate appropriate model 
nlmod <- lm(moral ~ secpay + I(pct_high_rel_imp^3), data=wvs)

## print results
stargazer(nlmod, digits=2, type="text")

