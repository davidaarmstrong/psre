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
                           TRUE ~ polrt), 
         pr_fac = as.factor(polrt), 
         pr_num = as.numeric(polrt))

## estimate models treating political rights as both 
## quantitative (modc) and categorical (modd)
modc <- lm(resemaval ~ pr_num, data=wvs1)
modd <- lm(resemaval ~ pr_fac, data=wvs1)

## use factorplot to identify the differences among
## coefficients for the pr_fac variable.
f <- factorplot(modd, factor.var="pr_fac", adjust.method = "none")

## use qvcalc to calculate the quasi variances
## for all levels of pr_fac (including the reference)
qpr <- qvcalc(modd, "pr_fac")

bpr <- c(0, coef(modd)[-1])

## make 95% quasi-confidence intervals
qci <- apply(outer(qt(.975, modd$df.residual)*qpr$qvframe[,3], c(-1,1), "*"), 
             2, function(x)bpr+x)
## organize confidence intervals and coefficients into 
## a data frame. 
qplot_dat <- data.frame(
  x =1:7, 
  y=bpr, 
  low = qci[,1], 
  up = qci[,2]
)

## make plot
ggplot(qplot_dat, aes(x=x, y=y, ymin=low, ymax=up)) + 
  geom_errorbar(width=.15) + 
  geom_point() + 
  theme_classic() + 
  labs(x="Political Rights", y="Predicted Emancipative Values")
ggsave("output/f6_2.png", height=4.5, width=4.5, units="in", dpi=300)
