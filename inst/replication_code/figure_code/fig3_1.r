## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages

library(tidyverse)
library(psre)
library(scales)

## load data from psre package
data(wvs)

## make a vector of codes that will serve as the labels for the
## factor civ
codes <- c("Other", "African", "Buddhist", "Hindu", "Islamic", "Japanese", 
           "Latin American", "Orthodox", "Sinic", "Western")

## make table 3.1, which will be used to construct the graph
tab3.1 <- wvs %>% 
  mutate(civ = factor(civ, labels=codes)) %>% 
  group_by(civ) %>%
  count %>% 
  ungroup %>%
  mutate(percent = round(n/sum(n)*100, 1)) %>% 
  na.omit %>% 
  arrange(-percent) %>% 
  setNames(c("Civilization", "Frequency", "Percentage"))

## construct the graph. 
## note that in the call to ggplot, we use the reorder() function to 
## change the ordering of the bars to be from largest to smallest. 
ggplot(tab3.1, 
       aes(x=reorder(Civilization, -Percentage, mean), y=Percentage/100)) + 
  geom_bar(stat="identity", fill="gray50", col="transparent") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  labs(x="", y="Percentage") + 
  scale_y_continuous(breaks = c(5, 10, 15, 20, 25)/100, labels=percent)
# ggssave("output/f3_1.png", height=4.5, width=4.5, units="in", dpi=300)


