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
qvpr <- qvcalc(modd, "pr_fac")$qvframe

## Using 0 as the estimate for the reference category 
## and the quasi-variances from the qvcalc procedure
## as the variance-covariance matrix, calculate the 
## optimal visual hypothesis testing confidence level 
## for the plot
bpr <- c(0, coef(modd)[-1])
vpr <- diag(qvpr[,4])
o <- optCL(modd, "pr_fac", b=bpr, v=vpr)

## the optCL function identifies confidence levels in the range
## (0.75, 0.85) as the appropriate levels (call these alpha). 
## we can find the appropriate confidence intervals by using 
## the critical t-value of (1-alpha)/2 for .75, .8 and .85, 
## which would be .87, .9 and .925, respectively. 
qci1 <- cbind(
  bpr - qt(.925, modd$df.residual)*qvpr[,3], 
  bpr + qt(.925, modd$df.residual)*qvpr[,3]
)
qci2 <- cbind(
  bpr - qt(.9, modd$df.residual)*qvpr[,3], 
  bpr + qt(.9, modd$df.residual)*qvpr[,3]
)
qci3 <- cbind(
  bpr - qt(.87, modd$df.residual)*qvpr[,3], 
  bpr + qt(.87, modd$df.residual)*qvpr[,3]
)

## make the data used in the plot
qplot_dat_opt <- data.frame(
  x =1:7, 
  y=c(bpr,bpr, bpr),  
  low = c(qci1[,1], qci2[,1], qci3[,1]),  
  up = c(qci1[,2], qci2[,2], qci3[,2]), 
  clevel = factor(rep(1:3, each=7), labels=c("85%", "80%", "74%"))
)

## A. 74%
qplot_dat_opt %>% filter(clevel == "74%") %>%
  ggplot( aes(x=x, y=y, ymin=low, ymax=up)) + 
  geom_errorbar(width=.15) + 
  geom_point() + 
  theme_classic() + 
  labs(x="Political Rights", y="Predicted Emancipative Values")
ggsave("output/f6_3a.png", height=4.5, width=4.5, units="in", dpi=300)

## B. 80%
qplot_dat_opt %>% filter(clevel == "80%") %>%
  ggplot( aes(x=x, y=y, ymin=low, ymax=up)) + 
  geom_errorbar(width=.15) + 
  geom_point() + 
  theme_classic() + 
  labs(x="Political Rights", y="Predicted Emancipative Values")
ggsave("output/f6_3b.png", height=4.5, width=4.5, units="in", dpi=300)

## C. 85%
qplot_dat_opt %>% filter(clevel == "85%") %>%
  ggplot( aes(x=x, y=y, ymin=low, ymax=up)) + 
  geom_errorbar(width=.15) + 
  geom_point() + 
  theme_classic() + 
  labs(x="Political Rights", y="Predicted Emancipative Values")
ggsave("output/f6_3c.png", height=4.5, width=4.5, units="in", dpi=300)

