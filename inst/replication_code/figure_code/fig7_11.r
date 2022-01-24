## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)

## load data from psre package
data(wvs)

## filter to only waves 2 and 3 of the data. 
## turn democrat into a factor with the appropriate labels
## keep only the first instance where the country appears in the data
wvs0 <- wvs %>% 
  filter(wave %in% c(3,2)) %>% 
  mutate(democrat= factor(democrat, levels=1:2, 
                          labels=c("New Democracy", "Established Democracy"))) %>%
  group_by(country) %>% 
  arrange(wave) %>% 
  slice_head(n=1) 

## linear additive model 
lmod <- lm(secpay ~ gini_disp + democrat, data=wvs0)
## linear interactive model
imod <- lm(secpay ~ gini_disp*democrat, data=wvs0)

## calculate the density estimate and normal reference 
## distribution for the additive linear model 
nbd <- normBand(lmod$residuals, kernel="gaussian", bw="nrd0")

## A. Model 7a: linear
ggplot(nbd, aes(x=eval.points)) + 
  geom_ribbon(aes(ymin = lwr, ymax=upr), alpha=.25, fill="gray50") + 
  geom_ribbon(aes(ymin = lwd_od, ymax = upr_od), col="transparent", alpha=.5) + 
  geom_line(aes(y=obsden), col="black") + 
  theme_classic() + 
  labs(x="Residuals", 
       y="Density")
# ggssave("output/f7_11a.png", height=4.5, width=4.5, units="in", dpi=300)

## calculate the density estimate and normal reference 
## distribution for the linear interaction model 
nbd <- normBand(imod$residuals, kernel="gaussian", bw="nrd0")

## Model 7b: Interaction
ggplot(nbd, aes(x=eval.points)) + 
  geom_ribbon(aes(ymin = lwr, ymax=upr), alpha=.25, fill="gray50") + 
  geom_ribbon(aes(ymin = lwd_od, ymax = upr_od), col="transparent", alpha=.5) + 
  geom_line(aes(y=obsden), col="black") + 
  theme_classic() + 
  labs(x="Residuals", 
       y="Density")
# ggssave("output/f7_11b.png", height=4.5, width=4.5, units="in", dpi=300)



