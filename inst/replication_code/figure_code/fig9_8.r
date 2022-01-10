## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(ggridges)

## load data from psre package
data(wvs)

## reduce civ to 3 categories
wvs1 <- wvs %>% 
  mutate(
    civ2 = case_when(
      civ == 9 ~ "Western", 
      civ == 6 ~ "Latin American", 
      TRUE ~ "Other"),
    civ2 = factor(civ2, levels=c("Western", "Latin American", "Other")), 
    democrat= factor(democrat, levels=1:2, 
                     labels=c("New Democracy", "Established Democracy")))


## A. Variation in Secondary Education by Civilization Group
ggplot(wvs1, aes(x=pct_secondary, y=civ2)) + 
  geom_density_ridges(scale=.9) + 
  stat_summary(aes(x=pct_secondary, group=civ2), fun.data = function(x)data.frame(
    ymin = quantile(x, .25, na.rm=TRUE), 
    ymax = quantile(x, .75, na.rm=TRUE)), geom="linerange", size=1.5) + 
  ## plot a narrow line in the violin plot between the 5th and 95th percentiles
  stat_summary(aes(x=pct_secondary, group=civ2), fun.data = function(x)data.frame(
    ymin = quantile(x, .05, na.rm=TRUE), 
    ymax = quantile(x, .95, na.rm=TRUE)), geom="linerange") + 
  ## put a white dot in the violin plot at the median 
  stat_summary(aes(x=pct_secondary, group=civ2), fun.data = function(x)data.frame(
    y = median(x, na.rm=TRUE)), geom="point", col="white", size=1) + 
  theme_classic() + 
  labs(y="", x="Proportion with Secondary Education") 
ggsave("output/f9_8a.png", height=4.5, width=4.5, units="in", dpi=300)


## find the terciles of pct_seoncary
q3 <- quantile(wvs1$pct_secondary, c(0,.33,.67, 1), na.rm=TRUE)
## reset the first and last values to appropriate ones - 
## the first one removes the zeroes, which are due to 
## missing data. 
q3[1] <- 0.02
q3[4] <- 0.69

## create a categorical pct_secondary variable that 
## is cut at the tercile values. 
wvs1 <- wvs1 %>% 
  mutate(sec_fac = cut(pct_secondary, breaks=q3))
levels(wvs1$sec_fac) <- c("Low", "Middle", "High")


## B. Variation in Civilization type by Secondary Education 
wvs1 %>% 
  filter(!is.na(sec_fac)) %>% 
ggplot(aes(x=sec_fac, fill=civ2)) + 
  geom_bar(position="dodge", col="white") +
  theme_classic() + 
  theme(legend.position="top") + 
  scale_fill_manual(values=c("gray25", "gray50", "gray75")) + 
  labs(x="Proportion with Secondary Education", y="Frequency", fill="")
ggsave("output/f9_8b.png", height=4.5, width=4.5, units="in", dpi=300)

