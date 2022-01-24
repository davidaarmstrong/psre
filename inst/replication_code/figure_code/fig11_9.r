## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(ordinal)
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

## ordered logit with piecewise linear spline for pr
opwl <- clm(pts_fac ~ BL(pr, .34) + BR(pr, .34) + cwar + iwar + log(rgdpe) + log(pop), data=repress)

## make ordered effect plot data
oa1 <- ordAveEffPlot(opwl, "pr", repress, nvals=41, plot=FALSE)

## make plot
ggplot(oa1$data, aes(x=s, y=mean, ymin=lower, ymax=upper)) + 
  geom_ribbon(alpha=.25) + 
  geom_line() + 
  facet_wrap(~as.factor(y), nrow=1) + 
  theme_bw() + 
  theme(panel.grid=element_blank()) + 
  labs(x="Political Rights", y="Predicted Pr(y=m)") 
# ggssave("output/f11_9.png", height=3, width=12, units="in", dpi=300)
