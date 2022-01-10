## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(ggeffects)
library(factorplot)
library(qvcalc)

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

## generate importance data using the srr_imp function 
## from the psre package. 
imp_plot_dat <- srr_imp(m7, wvs1, R=1500)

## add a variable to the plot data indicating which 
## variable corresponds to each importance figure. 
imp_plot_dat$variable <- factor(1:4, labels=c("Civilization", "GDP/capita", 
                                  "Post-Secondary Education", "Religious Society"))
  
## make plot
ggplot(imp_plot_dat, aes(x=reorder(variable, importance, mean), y=importance, 
                         ymin=lwr, ymax=upr)) + 
  geom_pointrange() + 
  theme_classic() + 
  labs(x="", y="Importance") + 
  coord_flip()
ggsave("output/f6_4.png", height=4.5, width=4.5, units="in", dpi=300)
