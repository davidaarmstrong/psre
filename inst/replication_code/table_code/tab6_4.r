## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(psre)
library(tidyverse)
library(stargazer)

## load data from psre package
data(wvs)

## Civilization labels
codes <- c("Other", "African", "Buddhist", "Hindu", "Islamic", "Japanese", 
           "Latin American", "Orthodox", "Sinic", "Western")

## recode civilization categories
wvs <- wvs %>% mutate(
  civ = case_when(
    civ == 4 ~ "Islamic", 
    civ == 6 ~ "Latin American", 
    civ == 7 ~ "Orthodox", 
    civ == 8 ~ "Sinic", 
    civ == 9 ~ "Western", 
    TRUE ~ "Other"), 
  civ = factor(civ, levels=c("Western", "Sinic", "Islamic", "Latin American", 
                             "Orthodox", "Other")), 
  ## create percent with secondary education or more
  pct_sec_plus = pct_secondary + pct_some_univ + pct_univ_degree, 
  ## create religious society variable
  rel_soc = factor(as.numeric(pct_high_rel_imp > .75), 
                   levels=c(0,1), labels=c("No", "Yes"))
)

wvs1 <- wvs %>% 
  ## create democratic history factor
  mutate(democrat= factor(democrat, levels=1:2, 
                          labels=c("New Democracy", 
                                   "Established Democracy"))) %>%
  ## keep only first instance of country in data
  group_by(country) %>% 
  arrange(wave) %>% 
  slice_head(n=1) %>% 
  ungroup %>% 
  arrange(democrat, gini_disp) %>%
  ## keep required variables
  select(civ, resemaval, gdp_cap, pop, rel_soc, 
         pct_sec_plus, polrt) %>% 
  ## listwise delete
  na.omit() %>% 
  ## replace political rights =1 if it is currently ""
  ## make factor and numeric versions of polrt
  mutate(polrt = case_when(polrt == "" ~ "1", 
                           TRUE ~ polrt), 
         pr_fac = as.factor(polrt), 
         pr_num = as.numeric(polrt))

## estimate model treating pr as numeric
m1 <- lm(resemaval ~ pr_num, data=wvs1)

## estimate model treating pr as factor
m2 <- lm(resemaval ~ pr_fac, data=wvs1)

## table
stargazer(m1, m2, digits=2, type="text")

## Incremental F-test
anova(m1, m2)





