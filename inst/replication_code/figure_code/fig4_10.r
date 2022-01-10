## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)

## load data from psre package
data(wvs)

## filter out secular values greater than 0.1 (to increase resolution in the graph)
wvsa <- wvs %>% dplyr::filter(sacsecval > .1)

## make the plot
ggplot(wvsa, aes(x=gdp_cap, y=sacsecval)) + 
  geom_point(shape=1, size=2) + 
  geom_smooth(method="lm", linetype=1) + 
  theme_classic() + 
  labs(x="GDP/capita", y="Secular Values")
ggsave("output/f4_10a.png", height=4.5, width=4.5, units="in", dpi=300)


ggplot(wvsa, aes(x=gdp_cap, y=sacsecval)) + 
  geom_point(shape=1, size=2) + 
  geom_smooth(aes(col="Linear", linetype="Linear"), 
              method="lm", se=FALSE) + 
  geom_smooth(aes(col="Non-parametric", linetype="Non-parametric"), 
              method="loess", se=FALSE) + 
  scale_color_manual(values=c("black", "gray40")) + 
  scale_linetype_manual(values=c(1, 4)) + 
  theme_classic() + 
  theme(legend.position = c(.8, .15)) + 
  labs(x="GDP/capita", y="Secular Values", colour="Model", linetype="Model")
ggsave("output/f4_10b.png", height=4.5, width=4.5, units="in", dpi=300)
