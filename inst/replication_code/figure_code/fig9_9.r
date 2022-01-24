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
wvs2 <- wvs %>% 
  filter(pct_univ_degree > 0) %>%
  filter(!is.na(pct_high_rel_imp) & !is.na(gini_disp))

## calculat the bivariate density of gini and religious importance
d <- MASS::kde2d(x=wvs2$pct_high_rel_imp, y=wvs2$gini_disp, n=100)

## make a data frame out of the density estimates
d_dat <- data.frame(
  x = rep(d$x, length(d$y)), 
  y = rep(d$y, each = length(d$x)), 
  z = c(d$z)
)

## A. Bivariate kernel density
ggplot(d_dat, aes(x=x, y=y, fill=z)) + 
  geom_tile() + 
  theme_classic() + 
  theme(legend.position="top") + 
  scale_fill_gradient(low="gray85", high="black", na.value = "white") + 
  labs(x="Religious Importance", y="Gini", 
       fill = "Density")
# ggssave("output/f9_9a.png", height=4.5, width=4.5, units="in", dpi=300)



## identify terciles of both variables
q33a <- quantile(wvs$pct_high_rel_imp, c(.33, .67), na.rm=TRUE)
q33b <- quantile(wvs$gini_disp, c(.33, .67), na.rm=TRUE)

## B. Bivariate binned density
ggplot(wvs, aes(x=pct_high_rel_imp, y=gini_disp)) +
  geom_bin2d(bins = 10) +
  geom_vline(xintercept=q33a, linetype=3, size=.75) + 
  geom_hline(yintercept=q33b, linetype=3, size=.75) + 
  scale_fill_gradient(low = "gray75", high = "black") +
  theme_classic() + 
  theme(legend.position= "top") + 
  labs(x="Religious Importance", y="Gini", fill="Frequency") 
# ggssave("output/f9_9b.png", height=4.5, width=4.5, units="in", dpi=300)

