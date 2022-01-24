## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(ggeffects)

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
                           TRUE ~ polrt), 
         pr_fac = as.factor(polrt), 
         pr_num = as.numeric(polrt))

## estimate models treating political rights as both 
## quantitative (m1) and categorical (m2)
m1 <- lm(resemaval ~ pr_num, data=wvs1)
m2 <- lm(resemaval ~ pr_fac, data=wvs1)

## get predicted values for both models at 
## the same values of political rights {1,2,3,4,5,6,7}
pc <- ggpredict(m1, "pr_num [1:7]")
pd <- ggpredict(m2, "pr_fac")

## Add a variable to the predictions indicating which kind 
## of variable was used - quantitative or categorical
pd <- pd %>% 
  mutate(x = as.numeric(x), 
         model = factor(2, 1:2, labels=c("Quantitative", "Categorical")))
pc <- pc %>% 
  mutate(model = factor(1, 1:2, labels=c("Quantitative", "Categorical")))

## put both prediction data frames together
pdat <- bind_rows(pc, pd)

## make plot
ggplot() + 
  geom_ribbon(data=filter(pdat, model=="Quantitative"), 
              aes(x=x, y=predicted, 
                  ymin = conf.low, 
                  ymax=conf.high),alpha=.25) +
  geom_line(data=filter(pdat, model=="Quantitative"), 
            aes(x=x, y=predicted)) + 
  geom_errorbar(data=filter(pdat, model=="Categorical"), 
                aes(x=x, y=predicted, 
                    ymin = conf.low, 
                    ymax=conf.high), width=.15) +
  geom_point(data=filter(pdat, model=="Categorical"), 
             aes(x=x, y=predicted)) + 
  theme_classic() + 
  theme(legend.position=c(.8, .9), 
        legend.text=element_text(size=10)) + 
  labs(x="Political Rights", y="Predicted Emancipative Values", 
       linetype="", fill="")
# ggssave("output/f6_1.png", height=4.5, width=4.5, units="in", dpi=300)

