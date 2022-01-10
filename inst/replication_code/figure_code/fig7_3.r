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

## Estimate model of secpay on gini and democrat
mod0 <- lm(secpay ~ gini_disp + democrat, data=wvs0)

## get partial residuals
cr <- residuals(mod0, type="partial")
colnames(cr) <- c("cr_gini", "cr_democrat")
## get the model frame for the model 
mf <- model.frame(mod0)
## append the partial residuals to the model frame data
mf <- cbind(mf, as.data.frame(cr))

## A. Gini Coefficient
ggplot(mf, aes(x=gini_disp, y=cr_gini)) + 
  geom_point(shape=1) + 
  geom_smooth(aes(linetype="Linear", colour="Linear"), method="lm",se=FALSE) + 
  geom_smooth(aes(linetype="Loess", colour="Loess"), se=FALSE, span=.85) + 
  scale_colour_manual(values=c("gray50", "black")) + 
  scale_linetype_manual(values=c(1,2)) + 
  theme_classic() + 
  theme(aspect.ratio=1, 
        legend.position=c(.85, .850), 
        legend.text=element_text(size=10)) + 
  labs(x="Gini Coefficient", 
       y="Component + Residual", 
       linetype="", colour="")
ggsave("output/f7_3a.png", height=4.5, width=4.5, units="in", dpi=300)

## B. Democratic history
ggplot(mf, aes(x=democrat, y=cr_democrat)) + 
  geom_boxplot() + 
  theme_classic() + 
  theme(aspect.ratio=1) + 
  labs(x="Democratic History", 
       y="Component + Residual")
ggsave("output/f7_3b.png", height=4.5, width=4.5, units="in", dpi=300)
