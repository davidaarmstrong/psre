## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(moderndive)

## load data from psre package
data(wvs)

## filter to only waves 2 and 3 of the data. 
## turn democrat into a factor with the appropriate labels
## keep only the first instance where the country appears in the data
wvs0 <- wvs %>% 
  filter(wave %in% c(3,2)) %>% 
  mutate(democrat= factor(democrat, levels=1:2, 
                          labels=c("New Democracy", "Established Democracy"))) %>%
  group_by(country) %>% 
  arrange(wave) %>% 
  slice_head(n=1) 

## make a dataset that only contains the two known outliers
## Czech Republic and Slovakia. 
crs <- wvs0 %>% filter(country %in% c("Czech Rep.", "Slovakia"))

## make plot
ggplot(wvs0, aes(x=gini_disp, y=secpay, colour=democrat)) + 
  geom_point(aes(shape=democrat)) + 
  geom_parallel_slopes(aes(linetype=democrat), 
                       se=FALSE) +   
  theme_classic() + 
  geom_text(x=crs$gini_disp[1]+.01, y=crs$secpay[1], label=crs$country[1], hjust=0, show.legend = FALSE) + 
  geom_text(x=crs$gini_disp[2]+.01, y=crs$secpay[2], label=crs$country[2], hjust=0, show.legend = FALSE) + 
  scale_colour_manual(values=c("gray50", "black")) + 
  scale_shape_manual(values=c(16, 1)) + 
  scale_linetype_manual(values=c(1,2)) + 
  theme(legend.position="top",
        legend.text = element_text(size=10), 
        aspect.ratio=1) + 
  labs(x="Gini Coefficient", y="Attitudes toward Inequality", 
       colour="", shape="", linetype="")
# ggssave("output/fig7_2.png", height=4.5, width=4.5, units="in", dpi=300)
