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

## omit missing values on gini_disp
## poly() will not accept missing data. 
wvs <- wvs %>% filter(!is.na(gini_disp))

## estimate the polynomial regression model
polymod <- lm(moral ~ poly(gini_disp, 2, raw=TRUE)  + I(pct_high_rel_imp^3), data=wvs)

## get predictions from the model
e <- ggpredict(polymod, "gini_disp")

## A. Fitted Values
ggplot() + 
  geom_ribbon(data = e, 
              aes(x=x, 
                  ymin = conf.low, 
                  ymax = conf.high), 
              alpha=.25) + 
  geom_line(data = e, 
            aes(x=x, 
                y=predicted)) + 
  geom_rug(data=wvs, aes(x=gini_disp)) + 
  theme_classic() + 
  labs(x="Gini Coefficient", 
       y="Moral Permissiveness Scale")
# ggssave("output/f8_5a.png", height=4.5, width=4.5, units="in", dpi=300)

## calculate the marginal effects using all the 
## values of gini_disp
b <- coef(polymod)
mes <- b[2] + 2*b[3]*wvs$gini_disp
ggplot(data=NULL, aes(x=mes)) + 
  geom_histogram(col="white", bins=15) + 
  labs(x="Marginal Effect", y="Frequency") + 
  geom_vline(xintercept = mean(mes), col="black", lty=2) + 
  theme_classic()
# ggssave("output/f8_5b.png", height=4.5, width=4.5, units="in", dpi=300)


