## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(psre)
library(tidyverse)
library(DAMisc)
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
  mutate(polrt = case_when(polrt == "" ~ "1", 
                           TRUE ~ polrt))

## rescale GDP values
wvs1 <- wvs1 %>% mutate(gdp_cap10 = gdp_cap/10000)

## estimate raw data model
m7 <- lm(resemaval ~ civ + gdp_cap10 + pct_sec_plus + rel_soc, data=wvs1)

## standardize quantitative variables and estimate model
m7a <- lm(resemaval ~ civ + gdp_cap10 + pct_sec_plus + rel_soc, data=scaleDataFrame(wvs1))

## standardize only quantitative x-variables
m7b <- wvs1 %>% 
  mutate(across(c(gdp_cap10, pct_sec_plus), ~c(scale(.x)))) %>% 
  lm(resemaval ~ civ + gdp_cap10 + pct_sec_plus + rel_soc, data=.)

## gelman standardization of quantitative x-variables
tmp2 <- wvs1 %>% 
  mutate(across(c(gdp_cap10, pct_sec_plus), ~.x/(2*sd(.x)))) 
m7c <- lm(resemaval ~ civ + gdp_cap10 + pct_sec_plus + rel_soc, data=tmp2)

## make a function to rescale variables in the range (0,1)
rs01 <- function(x){
  z <- x-min(x, na.rm=TRUE)
  z <- z/max(z, na.rm=TRUE)
  z
}

## rescale quantitative variables in (0,1) and estimate model
tmp1 <- wvs1 %>% 
  mutate(across(c(gdp_cap10, pct_sec_plus), ~rs01(.x)))
m7d <- lm(resemaval ~ civ + gdp_cap10 + pct_sec_plus + rel_soc, data=tmp1)

stargazer(m7, m7a, m7b, m7c, m7d, digits=3, type="text")


## calculate importance
## There is an error in the text - importance for post-secondary education 
## should be 0.01
srr_imp(m7, wvs1, R=1500)

