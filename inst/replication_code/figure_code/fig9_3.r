## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)

## load data from psre package
data(wvs)

## reduce civ to 3 categories
wvs1 <- wvs %>% 
  mutate(
    civ2 = case_when(
      civ == 9 ~ "Western", 
      civ == 6 ~ "Latin American", 
      TRUE ~ "Other"),
    civ2 = factor(civ2, levels=c("Western", "Latin American", "Other")), 
    democrat= factor(democrat, levels=1:2, 
                     labels=c("New Democracy", "Established Democracy")))

## estimate interaction model
intmod2 <- lm(resemaval ~ civ2 * pct_secondary, data=wvs1)

## calculate simple slopes
ss2 <- simple_slopes(intmod2, "pct_secondary", "civ2")
## find optimal visual testing confidence level 
o2 <- optCL(b=ss2$est$slope, v=ss2$v)
## save the optimal level
opt_lev <- mean(o2$opt_levels)
## find the appropriate t-critical value
tcrit <- qt(1-(1-opt_lev)/2, df=intmod2$df.residual)

## rename variables in the simple slopes data and 
## generate confidence intervals 
g <- ss2$est %>% 
  rename("predicted" = "slope", 
         "x" = "group") %>% 
  mutate(conf.low = predicted - tcrit*se, 
         conf.high = predicted + tcrit*se)

## make plot
ggplot(g, aes(y=reorder(x, predicted, mean), x=predicted, 
              xmin = conf.low, xmax=conf.high)) + 
  geom_errorbarh(height=.1) + 
  geom_point() + 
  theme_classic() + 
  labs(x="Simple Slopes of Secondary Education\n(85% Confidence Interval)", y="")
ggsave("output/f9_3.png", height=4.5, width=4.5, units="in", dpi=300)
