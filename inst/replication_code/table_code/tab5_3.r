## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(DAMisc)
library(stargazer)

## load data from psre package
data(wvs)

## select relevant variables, listwise delete and 
## generate pct_su_plus variable
tmp <- wvs %>% 
  mutate(pct_su_plus = pct_some_univ + pct_univ_degree)%>% 
  select(pct_secondary, pct_some_univ, pct_univ_degree, pct_su_plus, 
                             pct_low_income, pct_high_income, pct_female, 
                             mean_lr, resemaval, sacsecval, moral, democrat) %>% 
  na.omit() %>% 
  select(resemaval, pct_su_plus, pct_high_income, pct_female, democrat)

## Original Model
m4 <- lm(resemaval ~ pct_su_plus + pct_high_income + pct_female + democrat, data=tmp)

## quantitative variables normalized (mean=0, sd=1)
m4a <-  lm(resemaval ~ pct_su_plus + pct_high_income + pct_female + democrat, data=scaleDataFrame(tmp))

## x-standardized model
s2 <- tmp %>% mutate(across(pct_su_plus:pct_female, ~ c(scale(.x))))
m4b <-  lm(resemaval ~ pct_su_plus + pct_high_income + pct_female + democrat, data=s2)

## gelman approach (scaled by 2sd)
s3 <- tmp %>% mutate(across(pct_su_plus:pct_female, ~ (.x-mean(.x))/(2*sd(.x))))
m4c <-  lm(resemaval ~ pct_su_plus + pct_high_income + pct_female + democrat, data=s3)

## rescale all variables to range (0,1)
s4 <- tmp %>% mutate(across(pct_su_plus:pct_female, ~ {z <- .x-min(.x); z <- z/max(.x); z}))
m4d <-  lm(resemaval ~ pct_su_plus + pct_high_income + pct_female + democrat, data=s4)

## make table
stargazer(m4, m4a, m4b, m4c, m4d, digits=2, type="text")







