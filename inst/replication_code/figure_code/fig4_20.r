## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(psre)
library(tidyverse)

## load data from psre package
data(gss)

## keep education values >= 9 where party3 is not missing
gss %>% dplyr::filter(educ >= 9 & !is.na(party3)) %>% 
  ggplot(aes(x=educ, y=aid_scale)) + 
  geom_point(shape=1, col="gray50", position=position_jitter(width=.5)) + 
  facet_grid(sex ~ party3) + 
  geom_smooth(method="lm", col="black", se=FALSE) + 
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        aspect.ratio = 1) + 
  labs(x="Years of Formal Education", 
       y = "Aid Generosity Scale")
# ggssave("output/f4_20.png", height=5, width=7, dpi=300)
