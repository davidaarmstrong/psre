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

## Fit different models for the two IVs 
## linear interaction 
imod <- lm(secpay ~ gini_disp*democrat,data=wvs0)
## truncated power basis cubic spline in gini
smod3 <- lm(secpay ~ tpb(gini_disp, degree=3, knot_loc=.35) + democrat, data=wvs0)
## third-degree polynomial in gini
pmod3 <- lm(secpay ~ poly(gini_disp, 3) + democrat, data=wvs0)
## second-degree polynomial in gini
pmod2 <- lm(secpay ~ poly(gini_disp, 2) + democrat, data=wvs0)
## linear additive model 
lmod <- lm(secpay ~ gini_disp + democrat, data=wvs0)

## get partial residuals from linear model
f4 <- residuals(lmod, type="partial")[, 1]

## make data for plot
d1 <- data.frame(gini = model.frame(lmod)$gini_disp, cpr = f4)

ggplot(d1, aes(x=gini, y=cpr)) + 
  geom_smooth(aes(linetype="Linear"), method="lm", 
              se=FALSE, col="black") + 
  geom_smooth(aes(linetype="Spline"), method="lm", 
              formula = y ~ tpb(x, 3, knot_loc=.35), 
              se=FALSE, col="black", alpha=.35) + 
  geom_smooth(aes(linetype="Polynomial (3)"), method="lm", 
              formula = y ~ poly(x, 3), se=FALSE, col="black") + 
  geom_smooth(aes(linetype="Polynomial (2)"), method="lm", 
              formula = y ~ poly(x, 2), se=FALSE, col="black") + 
  geom_point(shape=1, col="black", alpha=.5) +
  scale_linetype_manual(values=c(1,3,2,4)) +
  theme_classic() + 
  theme(legend.position = "top") + 
  labs(x="Gini Coefficient", y="Component + Residual", linetype="")
# ggssave("output/f7_4.png", height=4.5, width=4.5, units="in", dpi=300)


