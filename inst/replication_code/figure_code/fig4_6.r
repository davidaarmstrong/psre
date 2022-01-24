## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(scales)
library(psre)
library(ggridges)

## load data from psre package
data(gss)

## filter to only those observations that have valid 
## data on party3 and aidhouse
gss <- gss %>% dplyr::filter(!is.na(party3) & !is.na(aid_scale))

## A. Filled Densities

ggplot(gss, aes(x=aid_scale, fill=party3)) + 
  geom_density(alpha=.25, col="transparent") + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="Preference Toward Generosity of Government Aid", y="Density", fill="") +   
  scale_fill_grey()
# ggssave("output/f4_6a.png", height=4.5, width=4.5, units="in", dpi=300)


ggplot(gss, aes(x=aid_scale, linetype=party3)) + 
  stat_density(geom="line", position="identity") + 
  theme_classic() + 
  theme(legend.position="top") + 
  scale_linetype_manual(values=c(2,3,1)) + 
  labs(x="Preference Toward Generosity of Government Aid", y="Density", linetype="")
# ggssave("output/f4_6b.png", height=4.5, width=4.5, units="in", dpi=300)
