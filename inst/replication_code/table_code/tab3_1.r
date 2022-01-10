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


tab3.1 <- wvs %>% 
  ## label civ categories
  mutate(civ = factor(civ, labels=codes)) %>% 
  group_by(civ) %>%
  ## calculate n for each group
  count %>% 
  ungroup %>%
  ## calculate percent from n
  mutate(percent = round(n/sum(n)*100, 1)) %>% 
  na.omit %>% 
  ## sort in percentage, descending
  arrange(-percent)
colnames(tab3.1) <- c("Civilization", "Frequency", "Percentage")

tab3.1
