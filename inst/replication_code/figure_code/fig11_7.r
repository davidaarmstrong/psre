## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(heatmapFit)
library(gridExtra)
library(mgcv)
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

## estimate logit model of bjp vote
mod1 <- glm(bjp ~  eth + sbc + educyrs + tb3 + urban + 
              anti_immigration, 
            data=india, family=binomial)

## create heatmapFit data
gh1 <- gg_hmf(model.response(model.frame(mod1)), fitted(mod1), method="loess")

## A. Model 11b
png("output/f11_7a.png", height=5.5, width=4.5, units="in", res=300)  
grid.arrange(gh1[[1]], gh1[[2]], heights=c(2,8), ncol=1)
dev.off()


## get the variables from mod1 and listwise delete
d <- get_all_vars(mod1, india) %>% na.omit()
## estimate the model with splines for anti-immigration
## and education
mod2 <- gam(bjp ~  eth + sbc + s(educyrs) + tb3 + urban + 
              s(anti_immigration), 
            data=d, family=binomial)

## create the heatmapFit data 
gh2 <- gg_hmf(model.response(model.frame(mod2)), fitted(mod2))

## B. Generalized Additive Model
png("output/f11_7b.png", height=5.5, width=4.5, units="in", res=300)
grid.arrange(gh2[[1]], gh2[[2]], heights=c(2,8), ncol=1)
dev.off()



