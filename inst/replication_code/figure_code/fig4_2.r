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

## make the plot
gss %>% dplyr::filter(!is.na(party3) & !is.na(aidhouse)) %>% 
  ggplot() +
  geom_mosaic(aes(x=product(aidhouse, party3), fill = aidhouse)) + 
  labs(x="", y="Government Should Provide Housing for Poor", 
       fill = "") + 
  scale_fill_grey() + 
  theme_classic() + 
  theme(legend.position="top") + 
  guides(fill = guide_legend(ncol=2)) 
ggsave("output/f4_2.png", height=5, width=4.5, units="in", dpi=300)
