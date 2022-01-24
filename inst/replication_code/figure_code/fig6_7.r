## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(factorplot)

## load data from psre package
data(wvs)

## make civilization factor, religious society variable 
## and percent with at least secondary education.
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
  pct_sec_plus = pct_secondary + pct_some_univ + pct_univ_degree, 
  rel_soc = factor(as.numeric(pct_high_rel_imp > .75), 
                   levels=c(0,1), labels=c("No", "Yes"))
)

## make democracy factor variable and retain only each country's 
## first observation in the data. 
wvs1 <- wvs %>% 
  mutate(democrat= factor(democrat, levels=1:2, 
                          labels=c("New Democracy", 
                                   "Established Democracy"))) %>%
  group_by(country) %>% 
  arrange(wave) %>% 
  slice_head(n=1) %>% 
  ungroup %>% 
  arrange(democrat, gini_disp) %>%
  dplyr::select(civ, resemaval, gdp_cap, pop, rel_soc, 
                pct_sec_plus, polrt) %>% 
  na.omit() %>% 
  ## replace political rights with 1 if it is missing
  mutate(polrt = case_when(polrt == "" ~ "1", 
                           TRUE ~ polrt))

## divide gdp_cap by 10000 so the coefficients do not 
## get too small. 
wvs1 <- wvs1 %>% mutate(gdp_cap10 = gdp_cap/10000)
## estimate model 
m7 <- lm(resemaval ~ civ + gdp_cap10 + pct_sec_plus + rel_soc, data=wvs1)

## create civ as a new factor with the appropriately ordered levels
wvs1a <- wvs1 %>% 
  mutate(civ = factor(as.character(civ), levels=c("Western", "Latin American", 
                                                  "Other", "Orthodox", 
                                                  "Sinic", "Islamic")))
## update model based on new data with civ ordered in a different way
m7u <- update(m7, data=wvs1a)

## create factorplot data for m7u using the civ variable. 
fp <- factorplot(m7u, factor.var="civ", adjust.methods="none")
#png("output/f6_7.png", height=4.5, width=4.5, units="in", res=300)
plot(fp, polycol = c("gray70", "white", "gray70"), 
     textcol=c("black", "gray50", "black"), 
     abbrev.char=100, 
     print.sig.leg=FALSE, 
     print.square.leg = FALSE, 
     print.se=FALSE)
#dev.off()
