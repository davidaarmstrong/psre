## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(DAMisc)
library(gridExtra)

## load data from psre package
data(india)

## Manage india election data
india <- india %>% 
  ## make urban a binary variable
  mutate(urban = case_when(
    urbrural %in% c(1,2) ~ 1, 
    urbrural %in% 3:5 ~ 0, 
    TRUE ~ NA_real_), 
    ## make urban and sbc factors
    urban = as.factor(urban), 
    sbc = as.factor(sbc), 
    ## make bjp a dummy variable indicating
    ## bjp vote
    bjp = case_when(
      in_prty == 2 ~ 1, 
      in_prty %in% c(1,3,4,5) ~ 0, 
      TRUE ~ NA_real_), 
    ## recode ethnicity into broader categories
    eth = case_when(
      in_ethn1 %in% 1:4 ~ "Hindu", 
      in_ethn1 %in% 5:7 ~ "Muslim", 
      in_ethn1 %in% 8:12 ~ "Other", 
      TRUE ~ NA_character_), 
    eth = as.factor(eth), 
    ## make topbot into a three-category variable
    tb3 = case_when(
      topbot %in% 1:3 ~ "Low", 
      topbot %in% 4:7 ~  "Middle", 
      topbot %in% 8:10 ~ "High", 
      TRUE ~ NA_character_), 
    tb3 = factor(tb3, levels=c("Low", "Middle", "High")))

## estimate logit model of bjp vote
mod1 <- glm(bjp ~  eth + sbc + educyrs + tb3 + urban + 
              anti_immigration, 
            data=india, family=binomial)

## create average effect plot data for
## the education variable
eff_ed <- aveEffPlot(mod1, varname="educyrs", data=india, 
                     nvals=50, return=c("ci", "sim"))

## make main panel.
g11_4a <- ggplot(eff_ed$ci, aes(x=s, y=mean, ymin=lower, ymax=upper)) + 
  geom_ribbon(alpha=.25) + 
  geom_line() + 
  theme_classic() + 
  labs(x="Education", y="Predicted Probability of BJP Vote") 

## make histogram to sit on top of 
## main panel. 
g11_4b <- ggplot(india, aes(x=educyrs)) + 
  geom_histogram(fill="gray75", col="white", bins=10) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(colour="transparent"),
        axis.text.y= element_text(colour="transparent"),
        axis.ticks.y = element_line(colour="transparent")) 

## put both figures together. 
#png("output/f11_4.png", height=5.5, width=4.5, units="in", res=300)  
grid.arrange(g11_4b, g11_4a, ncol=1, heights=c(2,8))
#dev.off()
