## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(DAMisc)

## load data from psre package
data(wvs)

## keep only those countries where there are some 
## people with a university degree
wvs2 <- wvs %>% filter(pct_univ_degree > 0)

## estimate model
intmod4 <- lm(resemaval ~ gini_disp*pct_high_rel_imp, data=wvs)

## generate data giving the conditional effects
## of each continuous variable given the values
## of the other. 
plot.dat <- DAintfun2(intmod4, c("gini_disp", "pct_high_rel_imp"), plot.type = "none")

## A. Gini coefficient given religious importance
## main panel
e1a <- ggplot(data=plot.dat, 
              aes(x=vals_pct_high_rel_imp, y = eff_gini_disp, ymin=low_gini_disp, ymax=up_gini_disp)) + 
  geom_ribbon(alpha=.25) + 
  geom_line() + 
  theme_classic() + 
  labs(x="Religious Importance", 
       y = "Conditional Effect of Gini Coefficient") 

## get range of y-axis from main panel
p1 <- ggplot_build(e1a)
rgy <- p1$layout$panel_params[[1]]$y.range
maxy <- rgy[1] + .3*diff(rgy)

## histogram for panel A
e1b <- ggplot(data=wvs, 
              aes(x=pct_high_rel_imp)) + 
  geom_histogram(bins=15, col="white") + 
  theme_void()
e1bgrob <- ggplotGrob(e1b)

## put histogram in main panel
e1 <- e1a + 
  geom_hline(yintercept=0, lty=2, col="gray50") + 
  annotation_custom(e1bgrob, ymax=maxy) 
e1
ggsave("output/f9_5a.png", height=4.5, width=4.5, units="in", dpi=300)

## B. Religious importance given gini coefficient
## proceed as above
e2a <- ggplot(data=plot.dat, 
              aes(x=vals_gini_disp, y = eff_pct_high_rel_imp, ymin=low_pct_high_rel_imp, ymax=up_pct_high_rel_imp)) + 
  geom_ribbon(alpha=.25) + 
  geom_line() + 
  theme_classic() + 
  labs(x="Gini Coefficient", 
       y = "Conditional Effect of Religious Importance") 
p2 <- ggplot_build(e2a)
rgy <- p2$layout$panel_params[[1]]$y.range
maxy <- rgy[1] + .3*diff(rgy)

e2b <- ggplot(data=wvs, 
              aes(x=gini_disp)) + 
  geom_histogram(bins=15) + 
  theme_void()
e2bgrob <- ggplotGrob(e2b)

e2 <- e2a + 
  geom_hline(yintercept=0, lty=2, col="gray50") + 
  annotation_custom(e2bgrob, ymax=maxy) 

e2
ggsave("output/f9_5b.png", height=4.5, width=4.5, units="in", dpi=300)


## Change in Significance

changeSig(intmod4, vars=c("gini_disp", "pct_high_rel_imp"))





