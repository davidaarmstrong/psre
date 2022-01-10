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

## recode civilization into three categories
## make the democratic history factor
wvs <- wvs %>% 
  mutate(
    civ2 = case_when(
      civ == 6 ~ "Latin American", 
      civ == 9 ~ "Western", 
      TRUE ~ "Other"), 
    civ2 = factor(civ2, levels=c("Western", "Latin American", "Other")), 
    democrat= factor(democrat, levels=1:2, 
                     labels=c("New Democracy", "Established Democracy")))

## estimate interactive model
intmod <- lm(resemaval ~ civ2 * democrat, data=wvs)

## print model 
stargazer(intmod, digits=2, type="text")
