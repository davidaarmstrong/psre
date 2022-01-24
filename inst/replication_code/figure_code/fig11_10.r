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

## arrange the effect plot data by the valuse of political rights 
## and then by the outcome variable
dat <- oa1$data %>% 
  arrange(s,y) %>% 
  group_by(s) %>% 
  ## create cumulative probabilities for each
  ## value of pr
  mutate(cprob = cumsum(mean), 
         yfac = factor(y, levels=5:1))

## A. No labels
ggplot(dat, aes(x=s, y=mean, fill=yfac)) + 
  geom_area(position="stack") + 
  scale_fill_grey(breaks=1:5) + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="Political Rights", 
       y="Cumulative Predicted Probability\nof State Repression", 
       fill="State\nRepression")
# ggssave("output/f11_10.png", height=5, width=4.5, units="in", dpi=300)


## add in labels for a particular value of political rights. 
d15 <- dat[which(round(dat$s, 3) == 0.15), ]
d15 <- d15 %>% 
  ungroup %>% 
  mutate(lcprob = lag(cprob), 
         lcprob = ifelse(is.na(lcprob), 0, lcprob)) %>% 
  rowwise() %>% 
  mutate(laby = mean(c(cprob,lcprob)))

## B. Labels for probabilities at PR=.15
ggplot() + 
  geom_area(data=dat, aes(x=s, y=mean, fill=yfac), position="stack") + 
  geom_errorbar(data=d15[1:4, ], aes(x=s, ymin=lcprob+.01, ymax=cprob-.01), width=.005) + 
  geom_errorbar(data=d15[5, ,drop=FALSE], aes(x=s, ymin=lcprob+.01, ymax=cprob-.01), width=.005, col="white") + 
  geom_text(data=d15[1:4, ], aes(x=s+.025, y=laby, label=sprintf("%.2f", mean))) + 
  geom_text(data=d15[5, ], aes(x=s-.025, y=laby, label=sprintf("%.2f", mean)), col="white") + 
  scale_fill_grey(breaks=1:5) + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="Political Rights", 
       y="Cumulative Predicted Probability\nof State Repression", 
       fill="State\nRepression")
# ggssave("output/f11_10b.png", height=5, width=4.5, units="in", dpi=300)


