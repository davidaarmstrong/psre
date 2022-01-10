## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(scales)

## load data from psre package
data(relig_imp)

## make plot
ggplot(relig_imp, aes(y=reorder(country, l, mean), x=relig_imp, fill=n)) + 
  geom_tile() + 
  theme_classic() + 
  theme(axis.text.x=element_text(angle=45, hjust=1), 
        axis.text.y=element_text(size=6), 
        legend.position="top") + 
  scale_fill_gradient(low="gray10", high="gray90", labels=percent) + 
  labs(x="Religious Importance", y="", fill="%")
ggsave("output/f4_7.png", height=8, width=4, units="in", dpi=300)
