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

## make a data frame of counts by civilization code which 
## will be used to construct the graph
d <- wvs %>% filter(!is.na(civ)) %>% 
  mutate(civ = factor(civ, labels=codes)) %>% 
  group_by(civ) %>% 
  ## summarise produces counts by civilizationgroup
  summarise(value=n()) %>% 
  ungroup %>% 
  arrange(value) %>% 
  ## here, we produce variables that will be used either to plot
  ## the pie pieces or the labels
  mutate(csval = sum(value) - cumsum(value), 
         pct = sprintf("%.0f%%", (value/sum(value))*100), 
         civ = reorder(civ, value, mean), 
         lab = paste(civ, " (", pct, ")", sep=""), 
         lab = reorder(factor(lab), value, mean), 
         ## mid is the middle between the current cumulative sum and the previous cumulative sum
         mid = csval + (lag(csval)-csval)/2,
         ## the first value will have no lag, so we hard code
         ## the appropriate value in for the first value. 
         mid = ifelse(is.na(mid), 157, mid))  

## to make a pie chart in ggplot, we make a bar plot and then 
## transform it into polar coordinates
d %>% ggplot(aes_string(x="1", y="value", fill="lab")) + 
  geom_bar(stat="identity", show.legend = FALSE) + 
  geom_text(aes(x = 1.5, y=mid, label=lab),
            hjust=c(.25,1,1,1,1,1,1,1,0,0), 
            nudge_x=c(.25, .15, 0.05,0,0,0,0,0,0,0)) + 
  geom_segment(aes(x=1.45, xend=1.7, y=157, yend=157)) + 
  geom_segment(aes(x=1.45, xend=1.6, y=154, yend=154)) + 
  xlim(.55, 2.25) + 
  coord_polar("y", start=0) + 
  theme_void()
# ggssave("output/f3_2.png", height=4.5, width=4.5, units="in", dpi=300)

