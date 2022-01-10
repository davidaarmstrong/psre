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

## keep only those countries with higher than 0 
## percent university degree
wvs2 <- wvs %>% filter(pct_univ_degree > 0)

## estimate interaction model
intmod4 <- lm(resemaval ~ gini_disp*pct_high_rel_imp, data=wvs)

## print model
stargazer(intmod4, digits=2, type="text")
