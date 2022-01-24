## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)

## load data from psre package
data(wvs)

## keep only observations in waves 2 and 3
## keep only variables secpay, gini_disp, democrat, country and wave
## listwisde delete the data
## take only the first wave (either 2 or 3) where the country appears
## in the resulting data
wvsa <- wvs %>% dplyr::filter(wave %in% c(2,3)) %>% 
  select(secpay, gini_disp, democrat, country, wave) %>% 
  na.omit() %>% 
  arrange(country, wave) %>% 
  group_by(country) %>% 
  slice(1) %>% 
  mutate(democrat = factor(democrat, levels=1:2,  labels=c("New Democracy", "Established Democracy")), 
         lab = case_when(country %in% c("Czech Rep.", "Slovakia") ~ country, 
                         TRUE ~ NA_character_))

## A. Additive
ggplot(wvsa, aes(x=gini_disp, y=secpay)) + 
  geom_point(shape=1, size=2) + 
  geom_text(aes(label=lab), hjust=0, position=position_nudge(x=.025)) + 
  geom_smooth(aes(linetype="OLS"), method="lm", col="black", se=FALSE) + 
  geom_smooth(aes(linetype="NPR"), method="loess", col="black", size=.75, se=FALSE) + 
  labs(x="Observed Inequality", y="Attitudes Toward Inequality", 
       colour="", shape="", linetype="") + 
  theme_classic() + 
  scale_linetype_manual(values=c(2,1)) + 
  theme(legend.position="top", 
        legend.key.size = grid::unit(3, "lines"))
# ggssave("output/f4_12a.png", height=4.5, width=4.5, units="in", dpi=300)



## B. Interaction
ggplot(wvsa, aes(x=gini_disp, y=secpay)) + 
  geom_point(aes(shape=democrat), size=2) + 
  geom_text(aes(label=lab), hjust=0, position=position_nudge(x=.025)) + 
  scale_shape_manual(values=c(1,16)) + 
  scale_linetype_manual(values=c(1,2)) + 
  geom_smooth(aes(linetype=democrat), method="lm", se=FALSE, col="black") + 
  labs(x="Observed Inequality", y="Attitudes Toward Inequality", 
       linetype="", shape="") + 
  theme_classic() + 
  theme(legend.position="top", 
        legend.key.size = grid::unit(3, "lines"))
# ggssave("output/f4_12b.png", height=4.5, width=4.5, units="in", dpi=300)


## C. Interaction without outliers
wvsa %>% dplyr::filter(!(country %in% c("Czech Rep.", "Slovakia"))) %>% 
  ggplot(aes(x=gini_disp, y=secpay)) + 
  geom_point(aes(shape=democrat), size=2) + 
  scale_shape_manual(values=c(1,16)) + 
  scale_linetype_manual(values=c(1,2)) + 
  geom_smooth(aes(linetype=democrat), method="lm", se=FALSE, col="black") + 
  labs(x="Observed Inequality", y="Attitudes Toward Inequality", 
       linetype="", shape="") + 
  theme_classic() + 
  theme(legend.position="top", 
        legend.key.size = grid::unit(3, "lines"))
# ggssave("output/f4_12c.png", height=4.5, width=4.5, units="in", dpi=300)
