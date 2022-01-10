## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(ggthemes)
library(ggrepel)

## load data from psre package
data(wvs)

## filter to only include Argentina, Chile and Brazil
wvsc <- wvs %>% dplyr::filter(country %in% c("Argentina", "Chile", "Brazil"))
## make a variable called "label" that has the country 
## but only if the year is 2006, otherwise it is missing. 
wvsc <- wvsc %>% 
  mutate(label = ifelse(year == 2006, country, NA_character_))

## make the plot
ggplot(wvsc, aes(x=year, y=secpay)) + 
  geom_line(aes(colour=country, linetype=country), show.legend=FALSE) + 
  geom_text_repel(aes(label=label), hjust=1, show.legend = FALSE, nudge_x = c(0,1,1)) + 
  theme_classic() + 
  theme(aspect.ratio=1) + 
  scale_colour_grey() + 
  labs(x="Year", y="Attitudes Toward Inequality", 
       colour="", linetype="") + 
  coord_cartesian(xlim=c(1990, 2009), expand=TRUE)
ggsave("output/f4_16.png", height=4.5, width=4.5, units="in", dpi=300)
