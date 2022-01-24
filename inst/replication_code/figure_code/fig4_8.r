## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(scales)

## load data from psre package
data(ces)

## remove people who voted for the BQ and Other parties
ces <- ces %>% select(vote, province) %>% 
  dplyr::filter(!(vote %in% c("BQ", "Other"))) %>% 
  mutate(vote = droplevels(vote))

## Calculate the number of observations in each province-vote 
## combination.  Use that data to calculate the proportion of 
## vote for each party by province
cestab <- ces %>% group_by(province, vote) %>% 
  summarise(n = n()) %>% 
  ungroup %>% 
  na.omit %>% 
  group_by(province) %>% 
  mutate(pct = n/sum(n)) %>% 
  ungroup %>% 
  mutate(province = as.factor(as.character(province)))

## Make plot
ggplot(cestab, aes(x=vote, y=province, fill=pct)) + 
  geom_tile() +
  scale_fill_gradient(low="gray90", high="gray10", labels = percent) +
  theme_classic() + 
  theme(legend.position="top") + 
  labs(y = "", x="Vote 2019", fill="%")
# ggssave("output/f4_8a.png", height=6, width=4.5, units="in", dpi=300)


## pivot the vote percentages to wide, giving one row for
## each province. 
cesw <- cestab %>% 
  dplyr::select(province, vote, pct) %>% 
  pivot_wider(names_from="vote", values_from="pct")
## turn the result into a data frame and make the rownames
## The province name, then remove province as a variable. 
cesw <- as.data.frame(cesw)
rownames(cesw) <- cesw$province
cesw <- cesw %>% select(-province)
## create a distance matrix from the wide-form percentage data
d <- dist(cesw)
## do a hierarchical clustering using the Ward.D2 agglomeration algorithm
h <- hclust(d, method="ward.D2")

## order the levels of province by the results of the 
## cluster analysis. 
cestab2 <- cestab %>% 
  mutate(province = factor(as.character(province), 
                             levels=h$labels[h$order]))

## make the plot
ggplot(cestab2, aes(x=vote, y=province, fill=pct)) + 
  geom_tile() +
  scale_fill_gradient(low="gray90", high="gray10", labels = percent) +
  theme_classic() + 
  theme(legend.position="top") + 
  labs(y = "", x="Vote 2019", fill="%")
# ggssave("output/f4_8b.png", height=6, width=4.5, units="in", dpi=300)
