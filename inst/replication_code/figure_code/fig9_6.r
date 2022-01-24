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

## keep only those countries where there are some 
## people with a university degree
wvs2 <- wvs %>% filter(pct_univ_degree > 0)

## estimate model
intmod4 <- lm(resemaval ~ gini_disp*pct_high_rel_imp, data=wvs)

## get predictions for gini at two extreme values of religious importance
g1 <- ggpredict(intmod4, terms=c("gini_disp [all]", "pct_high_rel_imp [0.037, 0.999]"))
## change the levels of g1 to minimum and maximum
levels(g1$group) <- c("Minimum", "Maximum")

## A. Effect of Gini coefficient for the minimum and maximum
## of religious importance. 
ggplot(g1, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high, fill=group, linetype=group)) + 
  geom_ribbon(alpha=.25) + 
  geom_line() + 
  theme_classic() + 
  theme(legend.position=c(.85, .85)) + 
  scale_fill_manual(values=c("gray25", "gray50")) + 
  labs(x="Gini", y="Predicted Emancipative Values", 
       linetype = "Religious\nImportance", 
       fill = "Religious\nImportance")
# ggssave("output/f9_6a.png", height=4.5, width=4.5, units="in", dpi=300)

## B. Effect of religious importance for the minimum and maximum of gini coefficient
g2 <- ggpredict(intmod4, terms=c("pct_high_rel_imp [all]", "gini_disp [0.175, 0.632]"))
levels(g2$group) <- c("Minimum", "Maximum")
ggplot(g2, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high, fill=group, linetype=group)) + 
  geom_ribbon(alpha=.25) + 
  geom_line() + 
  theme_classic() + 
  theme(legend.position=c(.85, .85)) + 
  scale_fill_manual(values=c("gray25", "gray50")) + 
  labs(x="Religious Importance", y="Predicted Emancipative Values", 
       linetype = "Gini", 
       fill = "Gini")
# ggssave("output/f9_6b.png", height=4.5, width=4.5, units="in", dpi=300)





