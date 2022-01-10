## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(stargazer)
library(car)

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

## Table 5.9: ANOVA for Interaction Model
a1 <- Anova(intmod, type="II")
a2 <- Anova(intmod, type="III")
a1 <- a1[1:3, ]
a2 <- a2[2:4, ]

## collate table data 
tab9_2 <- tibble(
  var = c("Civilization", "Democratic History", "Interaction"),
  F_II = a1$`F value`, 
  DF_II = a1$Df,
  pval_II = a1$`Pr(>F)`,
  F_III = a2$`F value`, 
  DF_III = a2$Df,
  pval_III = a2$`Pr(>F)`,
)

## print table
tab9_2



