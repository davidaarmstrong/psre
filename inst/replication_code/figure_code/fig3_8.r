## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages

library(tidyverse)
library(psre)

## load data from psre package
data(wvs)

## Create the plot
ggplot(wvs) + 
  geom_boxplot(aes(x="Box Plot", y=gdp_cap)) + 
  geom_violin(aes(x="Violin", y=gdp_cap), bw=2500, fill="gray75") + 
  ## plot a wide line in the violin plot between the 25th and 75th percentiles. 
  stat_summary(aes(x="Violin", y=gdp_cap), fun.data = function(x)data.frame(
    ymin = quantile(x, .25, na.rm=TRUE), 
    ymax = quantile(x, .75, na.rm=TRUE)), geom="linerange", size=2) + 
  ## plot a narrow line in the violin plot between the 5th and 95th percentiles
  stat_summary(aes(x="Violin", y=gdp_cap), fun.data = function(x)data.frame(
    ymin = quantile(x, .05, na.rm=TRUE), 
    ymax = quantile(x, .95, na.rm=TRUE)), geom="linerange") + 
  ## put a white dot in the violin plot at the median 
  stat_summary(aes(x="Violin", y=gdp_cap), fun.data = function(x)data.frame(
    y = median(x, na.rm=TRUE)), geom="point", col="white") + 
  theme_classic() + 
  labs(x="", y="GDP/capita")
ggsave("output/f3_8.png", height=4.5, width=4.5, units="in", dpi=300)
