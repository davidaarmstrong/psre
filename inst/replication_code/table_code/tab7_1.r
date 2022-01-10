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
  slice_head(n=1) 

## estimate regression model
mod <- lm(secpay ~ gini_disp + democrat, data=wvs0)

## print model
stargazer(mod, digits=2, type="text")
