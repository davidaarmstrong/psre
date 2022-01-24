## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(ggplot2)
library(car)
library(psre)
library(ggeffects)

## load data from psre package
data(wvs)

## estimate model with non-linear transformation
nlmod <- lm(moral ~ secpay + I(pct_high_rel_imp^3), data=wvs)

## generate predictions from the model
g <- ggpredict(nlmod, "pct_high_rel_imp")

## naje graph
ggplot() + 
  geom_ribbon(data=g, aes(x=x, y=predicted, 
                          ymin=conf.low, ymax=conf.high), 
              alpha=.25) + 
  geom_line(data=g, aes(x=x, y=predicted)) + 
  geom_rug(data=wvs, aes(x=pct_high_rel_imp)) + 
  labs(x="Proportion for Whom Religion is Very Important", 
       y="Moral Permissiveness Scale") + 
  theme_classic()
# ggssave("output/f8_2.png", height = 4.5, width=4.5, units="in", dpi=300)

