## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)

## load data from psre package
data(ces)

## A. Raw data
ggplot(ces, aes(x=leader_lib, y=leader_con)) + 
  geom_point(shape=1, col="gray55") + 
  geom_smooth(aes(col="Linear", linetype="Linear"), 
              method="lm", se=FALSE) + 
  geom_smooth(aes(col="Non-parametric", linetype="Non-parametric"), 
              method="loess", se=FALSE) + 
  scale_color_manual(values=c("black", "gray40")) + 
  scale_linetype_manual(values=c(1, 4)) + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="Liberal Leader Feeling Thermometer\n(Justin Trudeau)", 
       y="Conservative Leader Feeling Thermometer\n(Andrew Scheer)", 
       colour="Model", linetype="Model")
ggsave("output/f4_11a.png", height=4.5, width=4.5, units="in", dpi=300)

# B. Jittered Data
ggplot(ces, aes(x=leader_lib, y=leader_con)) + 
  ## note that to jitter, we can use the position_jitter() function.
  geom_point(shape=1, col="gray55", 
             position=position_jitter(height=1.5, width=1.5)) + 
  geom_smooth(aes(col="Linear", linetype="Linear"), 
              method="lm", se=FALSE) + 
  geom_smooth(aes(col="Non-parametric", linetype="Non-parametric"), 
              method="loess", se=FALSE) + 
  scale_color_manual(values=c("black", "gray40")) + 
  scale_linetype_manual(values=c(1, 4)) + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="Liberal Leader Feeling Thermometer\n(Justin Trudeau)", 
       y="Conservative Leader Feeling Thermometer\n(Andrew Scheer)", 
       colour="Model", linetype="Model")
ggsave("output/f4_11b.png", height=4.5, width=4.5, units="in", dpi=300)
