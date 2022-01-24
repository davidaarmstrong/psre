## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(ggplot2)
library(car)
library(psre)
library(ggeffects)
library(ggrepel)

## load data from psre package
data(wvs)

## estimate the non-linear transformation model
nlmod <- lm(moral ~ secpay + I(pct_high_rel_imp^3), data=wvs)

## Using the average case approach
## get coefficient from model. 
b2 <- coef(nlmod)
## find average of independent variable
xval <- mean(wvs$pct_high_rel_imp, na.rm=TRUE)
## calculate the marginal effect at average value
ave_case_me <- 3*b2[3] *xval^2


## Using the observed case approach
## the x-values are all the values of the independent variable
xvals <- na.omit(wvs$pct_high_rel_imp)
## calculate the marginal effects at all of the values of the
## independent variable. 
mes <- 3*b2[3]*xvals^2
ave_me <- mean(mes)

ggplot() + 
  geom_histogram(aes(x=mes), col="white", bins=12) + 
  geom_segment(aes(x=ave_case_me, xend=ave_case_me, y=0, yend=19.5), 
               size=1.25, linetype=2) + 
  geom_segment(aes(x=ave_me, xend=ave_me, y=0, yend=19.5), 
               size=1.25, 
               linetype=3) + 
  geom_text_repel(aes(x=ave_case_me, y=19.5, label="Average Case"), 
                  hjust= 0, vjust=1, nudge_x = .025, nudge_y=1) + 
  geom_text_repel(aes(x=ave_me, y=19.5, label="Observed Case"), 
                  hjust= 1, vjust=1, nudge_x=-.025, nudge_y=1) + 
  theme_classic() + 
  labs(x="Marginal Effect", y="Frequency")
# ggssave("output/f8_4.png", height = 4.5, width=4.5, units="in", dpi=300)



