## The contrasts can be made by simply including the factor civ
## in the model, but to make the table work appropriately, 
## I made the individual dummy or deviation regressors. 

## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(psre)

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


## make dymmy coding regressors - as noted above, 
## this is really to make the table look nice
## R will do this for you automatically if 
## civ is a factor
wvs1 <- wvs1  %>% 
  mutate(Islamic = ifelse(civ == "Islamic", 1, 0), 
         Western = ifelse(civ == "Western", 1, 0), 
         LatinAmerican = ifelse(civ == "Latin American", 1, 0), 
         Sinic = ifelse(civ == "Sinic", 1, 0), 
         Orthodox = ifelse(civ == "Orthodox", 1, 0), 
         Other = ifelse(civ == "Other", 1, 0))


## Run dummy coded models
m1 <- lm(resemaval ~ Sinic + Islamic + LatinAmerican + Orthodox + Other, 
         data=wvs1)
m2 <- lm(resemaval ~ Western + Islamic + LatinAmerican + Orthodox + Other, 
         data=wvs1)


## Create deviation coded regressors.  Again, R 
## will do this automatically if civ is a factor
## and has contr.sum contrasts. 
wvsd1 <- wvs1  %>% 
  mutate(
    Islamic = case_when(
      civ == "Islamic" ~ 1, 
      civ == "Western" ~ -1, 
      TRUE ~ 0), 
    LatinAmerican = case_when(
      civ == "Latin American" ~ 1, 
      civ == "Western" ~ -1, 
      TRUE ~ 0), 
    Sinic = case_when(
      civ == "Sinic" ~ 1, 
      civ == "Western" ~ -1, 
      TRUE ~ 0), 
    Orthodox = case_when(
      civ == "Orthodox" ~ 1, 
      civ == "Western" ~ -1, 
      TRUE ~ 0), 
    Other = case_when(
      civ == "Other" ~ 1, 
      civ == "Western" ~ -1, 
      TRUE ~ 0))

## run model 
m3 <- update(m1, data=wvsd1)

## deviation coded regressors with different baseline
wvsd2 <- wvs1  %>% 
  mutate(
    Islamic = case_when(
      civ == "Islamic" ~ 1, 
      civ == "Sinic" ~ -1, 
      TRUE ~ 0), 
    LatinAmerican = case_when(
      civ == "Latin American" ~ 1, 
      civ == "Sinic" ~ -1, 
      TRUE ~ 0), 
    Western = case_when(
      civ == "Western" ~ 1, 
      civ == "Sinic" ~ -1, 
      TRUE ~ 0), 
    Orthodox = case_when(
      civ == "Orthodox" ~ 1, 
      civ == "Sinic" ~ -1, 
      TRUE ~ 0), 
    Other = case_when(
      civ == "Other" ~ 1, 
      civ == "Sinic" ~ -1, 
      TRUE ~ 0))

m4 <- update(m2, data=wvsd2)

## Table
stargazer(m1, m2, m3, m4, 
          digits=3, type="text", 
          order=c(2,1,3:7))
