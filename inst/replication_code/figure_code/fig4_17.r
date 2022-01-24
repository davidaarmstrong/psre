## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(ggthemes)

## load data from psre package
data(wvs)

## keep only countries that have at least three data points
wvsd <- wvs %>% 
  group_by(country) %>%
  dplyr::filter(!is.na(secpay)) %>% 
  mutate(n = n()) %>% 
  dplyr::filter(n >=3) 

## A. Superposed Lines
ggplot(wvsd, aes(x=year, y=secpay, colour=country)) + 
  geom_line() + 
  scale_colour_grey() + 
  theme_classic() + 
  labs(x="Year", y="Attitudes Toward Inequality", 
       colour="")
# ggssave("output/f4_17a.png", height=4.5, width=5.5, units="in", dpi=300)



## B. Juxtaposed Lines
## note, we first, create the slope of the line relating secpay 
## to year.  We can then use this variable to order the panels. 
wvsd <- wvsd %>% 
  mutate(slope = coef(lm(secpay ~ year))[2])
ggplot(wvsd, aes(x=year, y=secpay)) + 
  geom_line() + 
  scale_colour_grey() + 
  facet_wrap(~reorder(country, slope, mean), nrow=3) + 
  theme_bw() + 
  theme(panel.grid=element_blank(),
        axis.text.x = element_text(angle=45, hjust=1)) + 
  labs(x="Year", y="Attitudes Toward Inequality", 
       colour="")
# ggssave("output/f4_17b.png", height=5, width=7, units="in", dpi=300)
