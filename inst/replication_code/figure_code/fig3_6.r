## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)

## load data from psre package
data(wvs)

## make data for qq plot
qqdf <- qqPoints(wvs$pct_mid_income)
a <- attr(qqdf, "ab")[1]
b <- attr(qqdf, "ab")[2]
l <- min(qqdf$theo) * b + a
u <- max(qqdf$theo) * b + a
## identify points that are outside of the confidence bounds 
## with a variable called 'outside'
qqdf <- qqdf %>% mutate(outside = factor(ifelse(x < lwr | x > upr, 2, 1)))

## make the QQ-plot
ggplot(qqdf, aes(x=theo, y=x)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15) + 
  geom_segment(aes(x=min(qqdf$theo), xend=max(qqdf$theo), y = l, yend=u)) + 
  geom_point(shape=1, show.legend = FALSE) + 
  theme_classic() + 
  labs(x="Theoretical Quantiles", 
       y="Observed Quantiles")
ggsave("output/f3_6.png", height=6, width=6, units="in", dpi=300)
