## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(dotwhisker)

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

## Using 0 as the estimate for the reference category, 
## calculate the optimal visual hypothesis testing 
## confidence level for the plot
bpr <- c(0, coef(m7)[-1])
vpr <- cbind(0, rbind(0, vcov(m7)[-1, -1]))
o <- optCL(m7, b=bpr, v=vpr)
ol7 <- mean(o$opt_levels)
## find critical-value for t-statistic
crit7 <- 1-(1-ol7)/2


## divide quantitative independent variables by 2sd
tmp2 <- wvs1 %>% 
  mutate(across(c(gdp_cap10, pct_sec_plus), ~.x/(2*sd(.x)))) 
## re-estimate model with standardized data
m7c <- lm(resemaval ~ civ + gdp_cap10 + pct_sec_plus + rel_soc, data=tmp2)

bpr <- c(0, coef(m7c)[-1])
vpr <- cbind(0, rbind(0, vcov(m7c)[-1, -1]))
o <- optCL(m7, b=bpr, v=vpr)
ol7c <- mean(o$opt_levels)
## find critical-value for t-statistic
crit7c <- 1-(1-ol7c)/2

## collect coefficients, standard errors and confidence intervals
term_dat <- tibble(
  term = colnames(model.matrix(m7))[-1], 
  estimate = coef(m7)[-1], 
  se =sqrt(diag(vcov(m7)))[-1], 
  conf.low = estimate - qt(crit7, m7$df.residual)*se, 
  conf.high = estimate + qt(crit7, m7$df.residual)*se
) %>% 
  ## remove se and rename terms, reorder based on size. 
  ## note that I reordered the coefficients by size within 
  ## civilization and then across the other variables. 
  dplyr::select(-se) %>% 
  mutate(term = c("Sinic", "Islamic", "Latin American", "Orthodox", 
                  "Other", "GDP/capita", "Post-Secondary\nEducation", 
                  "Religious Society")) 
term_dat <- term_dat[c(3,5,4,1,2,6,7,8), ]


## A. Raw Values
## the dwplot function is from the dotwhisker package. s
bracket <- list(c("Civilization\n(Reference=Western)", "Latin American", "Islamic"))
{dwplot(term_dat, 
        vline = geom_vline(xintercept=0, lty=2), 
        by_2sd = FALSE) +
    scale_colour_manual(values="black") + 
    scale_x_continuous(breaks=c(-.15,-.1, -.05, 0, 0.05)) + 
    theme_bw() + 
    theme(legend.position = "none", 
          panel.grid=element_blank()) + 
    labs(x = "Coefficient")} %>% 
  add_brackets(bracket) 
# ggssave("output/f6_5a.png", height=5, width=6.5, units="in", dpi=300)



term_dat <- tibble(
  term = colnames(model.matrix(m7c))[-1], 
  estimate = coef(m7c)[-1], 
  se =sqrt(diag(vcov(m7c)))[-1], 
  conf.low = estimate - qt(crit7c, m7c$df.residual)*se, 
  conf.high = estimate + qt(crit7c, m7c$df.residual)*se
) %>% 
  dplyr::select(-se) %>% 
  mutate(term = c("Sinic", "Islamic", "Latin American", "Orthodox", 
                  "Other", "GDP/capita", "Post-Secondary\nEducation", 
                  "Religious Society"))
term_dat <- term_dat[c(3,5,4,1,2,6,7,8), ]


## B. Gelman Standardized Values
bracket <- list(c("Civilization\n(Reference=Western)", "Latin American", "Islamic"))
{dwplot(term_dat, 
        vline = geom_vline(xintercept=0, lty=2), 
        by_2sd = FALSE) +
    scale_colour_manual(values="black") + 
    theme_bw() + 
    scale_x_continuous(breaks=c(-.15,-.1, -.05, 0, 0.05, 0.1)) + 
    theme(legend.position = "none", 
          panel.grid=element_blank()) + 
    labs(x = "Coefficient")} %>% 
  add_brackets(bracket) 
# ggssave("output/f6_5b.png", height=5, width=6.5, units="in", dpi=300)

