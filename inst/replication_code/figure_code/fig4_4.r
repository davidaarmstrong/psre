## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(scales)
library(psre)
library(ggmosaic)

## load data from psre package
data(gss)

## filter to only those observations that have valid 
## data on party3 and aidhouse
gss <- gss %>% dplyr::filter(!is.na(party3) & !is.na(aidhouse))

## A. Box plot
ggplot(gss, aes(x=party3, y=aid_scale)) + 
  geom_boxplot() + 
  theme_classic() + 
  labs(x="", y="Preference Toward Generosity of Government Aid") + 
  coord_flip()
ggsave("output/f4_4a.png", height=4.5, width=4.5, units="in", dpi=300)

## B. Violin Plot
ggplot(gss, aes(x=party3, y=aid_scale)) + 
  geom_violin(fill="gray75") + 
  ## plot a wide line in the violin plot between the 25th and 75th percentiles. 
  stat_summary(aes(x="Violin", y=aid_scale), fun.data = function(x)data.frame(
    ymin = quantile(x, .25, na.rm=TRUE), 
    ymax = quantile(x, .75, na.rm=TRUE)), geom="linerange", size=2) + 
  ## plot a narrow line in the violin plot between the 5th and 95th percentiles
  stat_summary(aes(x="Violin", y=aid_scale), fun.data = function(x)data.frame(
    ymin = quantile(x, .05, na.rm=TRUE), 
    ymax = quantile(x, .95, na.rm=TRUE)), geom="linerange") + 
  ## put a white dot in the violin plot at the median 
  stat_summary(aes(x="Violin", y=aid_scale), fun.data = function(x)data.frame(
    y = median(x, na.rm=TRUE)), geom="point", col="white") + 
  theme_classic() + 
  labs(x="", y="Preference Toward Generosity of Government Aid") + 
  coord_flip()
ggsave("output/f4_4b.png", height=4.5, width=4.5, units="in", dpi=300)

