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
  theme_classic() + 
  labs(x="GDP/capita", y="Secular Values")
ggsave("output/f4_9.png", height=4.5, width=4.5, units="in", dpi=300)
