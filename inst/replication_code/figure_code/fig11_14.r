## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(DAMisc)
## load data from psre package
data(repress)

## replace pr as NA if pr < 0
repress$pr <- ifelse(repress$pr < 0, NA, repress$pr)
## rescale pr to (0,1)
repress$pr <- repress$pr/100

## create transformed variables and turn binary 
## variables into factors
repress <- repress %>% mutate(log_gdp = log(rgdpe), 
                              logpop = log(pop),
                              pts_fac = as.factor(pts_s), 
                              cwar = as.factor(cwar), 
                              iwar = as.factor(iwar)) %>% 
  dplyr::select(pr, cwar, iwar, rgdpe, pop, log_gdp, logpop, pts_s, pts_fac) %>% 
  na.omit()

## make piecewise linear basis functions
BL <- function(x,c=.34)ifelse(x < c, c-x, 0)
BR <- function(x,c=.34)ifelse(x > c, x-c, 0)

## estimate multinomial logit model
mrm <-nnet::multinom(pts_fac ~ BL(pr, .34) + BR(pr, .34) + cwar + iwar +  
                       log(rgdpe) + log(pop), data=repress)


## make average effect plot data
oa1 <- mnlAveEffPlot(mrm, "pr", repress, nvals=41, plot=FALSE)

## arrange the effect plot data by the valuse of political rights 
## and then by the outcome variable
dat <- oa1 %>% 
  arrange(s,y) %>% 
  group_by(s) %>% 
  ## create cumulative probabilities for each
  ## value of pr
  mutate(cprob = cumsum(mean), 
         yfac = factor(y, levels=5:1))

## make plot
ggplot(dat, aes(x=s, y=mean, fill=yfac)) + 
  geom_area(position="stack") + 
  scale_fill_grey(breaks=1:5) + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="Political Rights", 
       y="Cumulative Predicted Probability\nof State Repression", 
       fill="State\nRepression")
ggsave("output/f11_14.png", height=5, width=4.5, units="in", dpi=300)

