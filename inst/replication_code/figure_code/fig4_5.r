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
gss <- gss %>% dplyr::filter(!is.na(educ) & !is.na(aid_scale))

gss <- gss %>% 
  ## replace education = 8 if education < 8
  mutate(educ8 = case_when(
                    educ < 8 ~ 8, 
                    TRUE ~ educ), 
  educ8 = as.factor(educ8)) %>% 
  dplyr::filter(!is.na(educ8)) 

## A. Violin Plot

ggplot(gss, aes(x=educ8, y=aid_scale)) + 
  geom_violin(fill="gray75") + 
  ## plot a wide line in the violin plot between the 25th and 75th percentiles. 
  stat_summary(fun.data= function(x)data.frame(
    ymin = quantile(x, .05, na.rm=TRUE), 
    ymax = quantile(x, .95, na.rm=TRUE)), 
    geom="linerange", size=.75) + 
  ## plot a narrow line in the violin plot between the 5th and 95th percentiles
  stat_summary(fun.data= function(x)data.frame(
    ymin = quantile(x, .25, na.rm=TRUE), 
    ymax = quantile(x, .75, na.rm=TRUE)), 
    geom="linerange", size=2) + 
  ## put a white dot in the violin plot at the median 
  stat_summary(fun.data= function(x)data.frame(
    y = median(x,na.rm=TRUE)), 
    geom="point", col="white") + 
  theme_classic() + 
  labs(x="Years of Formal Education Completed", y="Preference Toward Generosity of Government Aid") + 
  coord_flip()
ggsave("output/f4_5a.png", height=9, width=4.5, units="in", dpi=300)



## B. Ridgeline Plot
ggplot(gss, aes(x=aid_scale, y=educ8)) + 
  geom_density_ridges(scale=.9, colour="transparent") +
  ## plot a wide line in the violin plot between the 25th and 75th percentiles. 
  stat_summary(aes(x=aid_scale), fun.data = function(x)data.frame(
    ymin = quantile(x, .25, na.rm=TRUE), 
    ymax = quantile(x, .75, na.rm=TRUE)), geom="linerange", size=1.5) + 
  ## plot a narrow line in the violin plot between the 5th and 95th percentiles
  stat_summary(aes(x=aid_scale, group=educ8), fun.data = function(x)data.frame(
    ymin = quantile(x, .05, na.rm=TRUE), 
    ymax = quantile(x, .95, na.rm=TRUE)), geom="linerange") + 
  ## put a white dot in the violin plot at the median 
  stat_summary(aes(x=aid_scale, group=educ8), fun.data = function(x)data.frame(
    y = median(x, na.rm=TRUE)), geom="point", col="white", size=1) + 
  theme_classic() +
  labs(y="Years of Formal Education Completed", x="Preference Toward Generosity of Government Aid") 
ggsave("output/f4_5b.png", height=9, width=4.5, units="in", dpi=300)
