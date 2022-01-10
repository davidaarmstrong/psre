## Note, tables 10.1 and 10.2 are not statistical tables
## table 10.3 is the first statistical table in Ch 10.

## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(rio)
library(splines)
library(stargazer)

## load data from psre package
data(gss)

## rescale sei01 into the range (0,1)
## create sex factor
## recode education to 6 if it is 
## currently less than 6. 
## select required variables and listwise delete
gss <- gss %>% 
  mutate(sei01 = sei10/100, 
         sex = factorize(sex),
         educ = case_when(
           educ < 6 ~ 6, 
           TRUE ~ educ)) %>% 
  dplyr::select(childs, age, sei01, sex, educ) %>% 
  na.omit()

## estimate glm that is additive and linear (in the link) 
## for age
moda <- glm(childs ~ sei01 + sex + educ + age, 
            data=gss, family=poisson)

## estimate model with quadratic b-spline with 2 knots
## for age
modb <- glm(childs ~ sei01 + sex + educ + bs(age, degree=2, knots=c(43.5, 80.5)) ,
    data=gss, family=poisson)

## print models 
stargazer(moda, modb, digits=3, type="text")

