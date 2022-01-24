## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(ggeffects)
## you also need the multcomp package, but we are not 
## loading it using library() - instead we just call 
## functions from the namespace directly using multcomp::

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

## make a new hypothetical data frame that holds all other variables
## aside from civilization constant at central values and changes
## the levels of civilization.  
dat_star <- expand.grid(
  civ=levels(wvs1$civ), 
  gdp_cap10 = mean(wvs1$gdp_cap10, na.rm=TRUE), 
  pct_sec_plus = mean(wvs1$pct_sec_plus, na.rm=TRUE), 
  rel_soc = factor(2, levels=1:2, labels=c("No", "Yes")), 
  resemaval = .5
)

## generate the design matrix for m7 using the hypothetical
## data created above. 
Xstar <- model.matrix(m7, data=dat_star)
## calculate the predicted values using the design matrix above
eff <- Xstar %*% coef(m7)
## calculate the variance-covariance matrix of the effects
## calculated above. 
V <- Xstar %*% vcov(m7) %*% t(Xstar)

## use optCL to find the best visual testing confidence level
o1 <- optCL(b=eff, v=V, quasi_vars=NULL, add_ref = FALSE)
## use the mean of all acceptable confidence levels
opt_lev <- mean(o1$opt_levels)

## use the optimal confidence level in the cal to ggpredict()
e7 <- ggpredict(m7, "civ", ci.lvl = opt_lev)
ggplot(e7, aes(x=reorder(x, predicted, mean), y=predicted, ymin=conf.low, ymax=conf.high)) + 
  geom_errorbar(width=.15) + 
  geom_point() + 
  theme_classic() + 
  labs(x="", y="Prediced Emancipative Values\n(OVT Confidence Interval [79%])") + 
  coord_flip()
# ggssave("output/f6_6a.png", height=4.5, width=4.5, units="in", dpi=300)

## use 95% confiedence intervals
e7 <- ggpredict(m7, "civ", ci.lvl = 0.95)

## get the pairwise comparisons for the civilization variable
pwc <- summary(multcomp::glht(m7, linfct=multcomp::mcp(civ = "Tukey")), 
               test=multcomp::adjusted(type="none"))

## generate compact letter display data using cld()
cld1 <- multcomp::cld(pwc)

## extract the letter matrix from the compact letter display data
lmat <- cld1$mcletters$LetterMatrix

## make a plot of confidence intervals with the appropriate letter display
e7 <- e7 %>% mutate(x = reorder(x, predicted, mean))
letter_plot(e7, lmat) + 
  labs(x="Predicted Emancipative Values\n(95% Confidence Interval)")
# ggssave("output/f6_6b.png", height=4.5, width=4.5, units="in", dpi=300)

