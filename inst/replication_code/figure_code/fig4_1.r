## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(scales)
library(psre)

## load data from psre package
data(gss)

## filter to only those observations that have valid 
## data on party3 and aidhouse
gss <- gss %>% dplyr::filter(!is.na(party3) & !is.na(aidhouse))


## A. Stacked, not normalized
ggplot(gss, aes(x=party3, fill=aidhouse)) + 
  geom_bar() + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="", y="Frequency", fill="") + 
  scale_fill_grey()
ggsave("output/f4_1a.png", height=4.5, width=4.5, units="in", dpi=300)


## B. Stacked, normalized
## calculate percentages within for each value of 
## aidhouse within each category of party3
gss %>% 
  group_by(party3, aidhouse) %>% 
  summarise(n = n()) %>% 
  ungroup %>%
  group_by(party3) %>% 
  mutate(pct = n/sum(n)) %>% 
  ## make the plot
  ggplot(aes(x=party3, y=pct, fill=aidhouse)) + 
  geom_bar(stat = "identity") + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="", y="Percentage", fill="") + 
  scale_fill_grey() + 
  scale_y_continuous(labels=percent)
ggsave("output/f4_1b.png", height=4.5, width=4.5, units="in", dpi=300)


## C. Side-by-side bars, nor normalized
ggplot(gss, aes(x=party3, fill=aidhouse)) + 
  geom_bar(position="dodge") + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="", y="Frequency", fill="") + 
  scale_fill_grey()
ggsave("output/f4_1c.png", height=4.5, width=4.5, units="in", dpi=300)

## D. Side-by-side bars, normalized
## calculate percentages within for each value of 
## aidhouse within each category of party3
gss %>% 
  group_by(party3, aidhouse) %>% 
  summarise(n = n()) %>% 
  ungroup %>%
  group_by(party3) %>% 
  mutate(pct = n/sum(n)) %>% 
  ## make the plot
  ggplot(aes(x=party3, y=pct, fill=aidhouse)) + 
  geom_bar(stat = "identity", position="dodge") + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="", y="Percentage", fill="") + 
  scale_fill_grey() + 
  scale_y_continuous(labels=percent)
ggsave("output/f4_1d.png", height=4.5, width=4.5, units="in", dpi=300)

