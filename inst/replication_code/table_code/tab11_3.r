## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(DAMisc)
library(car)

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

## logit model without product term
mod1 <- glm(bjp ~  eth + educyrs + sbc + tb3 + urban + 
              anti_immigration, 
            data=india, family=binomial)

## logit model with product term
mod1i <- glm(bjp ~  eth*educyrs + sbc + tb3 + urban + 
               anti_immigration, 
             data=india, family=binomial)

## get y and predicted y from the model (additive)
y <- model.response(model.frame(mod1i))
yhat <- as.numeric(predict(mod1, type="response") > .5)

## make the cross-tabulation
tab <- table(yhat, y)
## print counts and column percentages
tabp <- sprintf("%.0f", tab)
ptab <- prop.table(tab, 2)*100
ptabp <- sprintf("(%.0f%%)", ptab)
## make into a data frame
tab11_3 <- tibble(
  Classification = c("Negative", "", "Positive", ""), 
  True_Negative = c(tab[1,1], ptabp[1], tab[2,1], ptabp[2]), 
  True_Positive = c(tab[1,2], ptabp[3], tab[2,2], ptabp[4])
)

tab11_3





