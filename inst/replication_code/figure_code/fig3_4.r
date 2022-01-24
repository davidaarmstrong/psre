## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages

library(tidyverse)
library(psre)
# library(reticulate)

## load data from psre package
data(wvs)

## filter out missing values on pct_mid_income
d2 <- wvs %>% 
  filter(!is.na(pct_mid_income))

## The normBand function creates data that will allow you to plot the
## normal confidence bands for the estimate and/or the 
## normal reference distribution. 

## A. Silverman (1986)
nbd <- normBand(d2$pct_mid_income, kernel="gaussian", bw="nrd0")

## create the plot using the nbd data frame
ggplot(nbd, aes(x=eval.points)) + 
  geom_ribbon(aes(ymin = lwr, ymax=upr), alpha=.25, fill="gray50") + 
  geom_ribbon(aes(ymin = lwd_od, ymax = upr_od), col="transparent", alpha=.5) + 
  geom_line(aes(y=obsden), col="black") + 
  theme_classic() + 
  labs(x="Proportion in Middle Income Category", 
       y="Density")
# ggssave("output/f3_4a.png", height=4.5, width=4.5, units="in", dpi=300)

## B. Scott (1992)
nbd <- normBand(d2$pct_mid_income, kernel="gaussian", bw="nrd")
ggplot(nbd, aes(x=eval.points)) + 
  geom_ribbon(aes(ymin = lwr, ymax=upr), alpha=.25, fill="gray50") + 
  geom_ribbon(aes(ymin = lwd_od, ymax = upr_od), col="transparent", alpha=.5) + 
  geom_line(aes(y=obsden), col="black") + 
  theme_classic() + 
  labs(x="Proportion in Middle Income Category", 
       y="Density")
# ggssave("output/f3_4b.png", height=4.5, width=4.5, units="in", dpi=300)


## C. Sheather and Jones (1991)
nbd <- normBand(d2$pct_mid_income, kernel="gaussian", bw="SJ")
ggplot(nbd, aes(x=eval.points)) + 
  geom_ribbon(aes(ymin = lwr, ymax=upr), alpha=.25, fill="gray50") + 
  geom_ribbon(aes(ymin = lwd_od, ymax = upr_od), col="transparent", alpha=.5) + 
  geom_line(aes(y=obsden), col="black") + 
  theme_classic() + 
  labs(x="Proportion in Middle Income Category", 
       y="Density")
# ggssave("output/f3_4c.png", height=4.5, width=4.5, units="in", dpi=300)

## D. Too Bumpy
nbd <- normBand(d2$pct_mid_income, kernel="gaussian", bw=0.02)
ggplot(nbd, aes(x=eval.points)) + 
  geom_ribbon(aes(ymin = lwr, ymax=upr), alpha=.25, fill="gray50") + 
  geom_ribbon(aes(ymin = lwd_od, ymax = upr_od), col="transparent", alpha=.5) + 
  geom_line(aes(y=obsden), col="black") + 
  theme_classic() + 
  labs(x="Proportion in Middle Income Category", 
       y="Density")
# ggssave("output/f3_4d.png", height=4.5, width=4.5, units="in", dpi=300)





