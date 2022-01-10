## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(GGally)

## load data from psre package
data(wvs)

## keep only the variables of interest in the investigation. 
tmp <- wvs %>% 
  select(resemaval, moral, pct_univ_degree, pct_female, pct_low_income, sacsecval) %>% 
  dplyr::filter(resemaval > 0 & moral > 0 & sacsecval > 0 ) %>% 
  setNames(., c("Emancipative\nValues", "Moral\nPermissiveness", 
                "% Univ Degree", "% Female", 
                "% Low Income", "Secular\nValues")) %>% 
  mutate(across(contains("pct"), ~.x*100))

## make the plot
g <- ggpairs(tmp, lower = list(continuous = wrap("smooth_loess", 
                                                 shape=1, 
                                                 size=1, 
                                                 col="gray65", 
                                                 linewidth=.5, 
                                                 se=FALSE)))
g + theme_bw() + theme(panel.grid=element_blank())
ggsave("output/f4_14.png", height=8, width=8, units="in", dpi=300)
