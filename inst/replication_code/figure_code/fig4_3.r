## This is a more complicated way of making mosaic plots, but one 
## that can be easily manipulated.  The main advantage to this way, 
## as opposed to using geom_mosaic() from the ggmosaic package is that
## it allows us to colour the blocks by other values not plotted - like
## The standardized residuals.

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

## use chisq.test() to calculate standardized residuals
sres <- chisq.test(table(gss$aidhouse, gss$party3))$stdres
sres <- as.data.frame(sres) %>% 
  setNames(c("aidhouse", "party3", "res")) 

## merge standardized residuals data onto original data
mpdata <- gss %>% 
  group_by(aidhouse, party3) %>% 
  summarise(n=n()) %>% 
  left_join(., sres) %>% 
  ## create 6 categories of residuals to make the shading easier. 
  mutate(cres = case_when(res < -3 ~ -3, 
                          res >= -3 & res < -2 ~ -2, 
                          res >= -2 & res < 0 ~ -.25, 
                          res >= 0 & res < 2 ~ .25, 
                          res >= 2 & res < 3 ~ 2, 
                          res >= 3 ~ 3)) %>% 
  ungroup %>% 
  group_by(party3) %>% 
  mutate(pct = n/sum(n)) %>% 
  na.omit()

## create a vector of widths for the bars - 
## the since there are three categories, we use 
## a total width of 2.9 (a little less than 3)
## that we divvy up among the categories based 
## on the percentage of observations in each 
## of the three groups on the x-axis. 
wds <- mpdata %>% 
  group_by(party3) %>% 
  summarise(n=sum(n)) %>% 
  ungroup %>% 
  mutate(pct = 2.9*n/sum(n)) %>% 
  dplyr::select(pct) %>% 
  pull

## We identify the mid-points of the bars on the x-axis. 
x <- c(0, rep(NA, length(wds)-1))
for(i in 2:length(wds)){
  ## loop over the values of wds
  ## take the previous x-value, 
  ## add half of the width for the previous category
  ## add half of the width for the current category 
  ## and 0.025 - the space between the bars. 
  x[i] <- x[(i-1)] + .5*wds[(i-1)] + .5*wds[i] + .025
}

## get the percentages of the variable on the y-axis, 
## for the first category on the x-axis. 
y <- mpdata %>% 
  filter(party3 == "Democrat") %>% 
  ungroup %>% 
  dplyr::select(pct) %>% 
  pull

## Start with zero and then calculate the cumulative 
## sum of y and append that to the starting zero. 
yc <- c(0, cumsum(y))
## calculate the midpoint between each adjacent pair of values
y <- yc[1:4] + diff(yc)/2
## get the labels for the values on the y-axis. 
ylevs <- levels(gss$aidhouse)  

## join together standardized residual data from above to the 
## x-values data created above. 
mpdata <- left_join(mpdata, 
                    data.frame(party3 = levels(mpdata$party3), x=x)) %>% 
  ## turn the standardized residuals into factors with appropriate labels. 
  mutate(cresf = factor(cres, levels=c(-3,-2,-.25,.25,2,3), 
                        labels=c("-3 < e", "-3 < e < -2", "-2 < e < 0", 
                                 "0 < e < 2", "2 < e < 3", "e < 3")),
         ## turn aidhouse and party3 into factors, so the labels show
         ## up in the appropriate order. 
         aidhouse = factor(aidhouse, levels=c("Definitely Yes", "Probably Yes",
                                              "Probably No", "Definitely No")), 
         party3 = factor(party3, levels=c("Democrat", "Other", "Republican")))

## make the plot
ggplot(mpdata, aes(x=x, y=pct, fill=cres)) + 
  geom_col(position="stack", width=wds, col="white") + 
  scale_fill_gradient(low="gray10", high="gray90") + 
  scale_x_continuous(breaks = x, labels=levels(gss$party3)) + 
  scale_y_continuous(breaks = y, labels=ylevs) + 
  labs(x="Party ID", y="", fill="Standardized\nResiduals") + 
  theme_classic()
ggsave("output/f4_3.png", height=4.5, width=5, units="in", dpi=300)
