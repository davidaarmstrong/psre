## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)

## load data from psre package
data(india)

## Manage india election data
india <- india %>% 
  ## make urban a binary variable
  mutate(urban = case_when(
    urbrural %in% c(1,2) ~ 1, 
    urbrural %in% 3:5 ~ 0, 
    TRUE ~ NA_real_), 
    ## make urban and sbc factors
    urban = as.factor(urban), 
    sbc = as.factor(sbc), 
    ## make bjp a dummy variable indicating
    ## bjp vote
    bjp = case_when(
      in_prty == 2 ~ 1, 
      in_prty %in% c(1,3,4,5) ~ 0, 
      TRUE ~ NA_real_), 
    ## recode ethnicity into broader categories
    eth = case_when(
      in_ethn1 %in% 1:4 ~ "Hindu", 
      in_ethn1 %in% 5:7 ~ "Muslim", 
      in_ethn1 %in% 8:12 ~ "Other", 
      TRUE ~ NA_character_), 
    eth = as.factor(eth), 
    ## make topbot into a three-category variable
    tb3 = case_when(
      topbot %in% 1:3 ~ "Low", 
      topbot %in% 4:7 ~  "Middle", 
      topbot %in% 8:10 ~ "High", 
      TRUE ~ NA_character_), 
    tb3 = factor(tb3, levels=c("Low", "Middle", "High")))

## estimate LPM of bjp vote
mod1l <- lm(bjp ~  eth + sbc + educyrs + tb3 + urban + 
              anti_immigration, data=india)
## estimate logit model of bjp vote
mod1 <- glm(bjp ~  eth + sbc + educyrs + tb3 + urban + 
              anti_immigration, 
            data=india, family=binomial)

## print both models
stargazer(mod1l, mod1, digits=2, type="text")



