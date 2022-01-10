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

## calculate the average first difference for each of the 
## variables in the model 
o1 <- ordChange2(opwl, c("pr", "cwar", "iwar", "rgdpe", "pop"), 
                 data=repress, diffchange="sd", n=2)


## collate results fromthe ordChange2 function
mean_dat <- as.data.frame(o1$diffs$mean)
colnames(mean_dat) <- paste0("mean_", 1:5)
lower_dat <- as.data.frame(o1$diffs$lower)
colnames(lower_dat) <- paste0("lower_", 1:5)
upper_dat <- as.data.frame(o1$diffs$upper)
colnames(upper_dat) <- paste0("upper_", 1:5)
all_dat <- bind_cols(mean_dat, lower_dat, upper_dat) %>% 
  mutate(var = rownames(mean_dat)) %>% 
  pivot_longer(-var, names_pattern = "(.*)_(.*)", 
               names_to=c(".value", "y"))

## rename the var factor levels to something better
## for plotting
all_dat <- all_dat %>% 
  mutate(varlab = factor(var, levels=c("cwar: 1-0", "iwar: 1-0", "pop", "pr", "rgdpe"), 
                         labels=c("Civil War", "Interstate War", 
                                  "Population", "Political Rights", "GDP")))

all_dat$varlab <- reorder(all_dat$varlab, ifelse(all_dat$y == 1, all_dat$mean, 0), mean)

## Make Plot
ggplot(all_dat, aes(x=mean, xmin=lower, xmax=upper, y=varlab)) + 
  geom_vline(xintercept=0, lty=3) + 
  geom_errorbarh(height=0) + 
  geom_point() + 
  facet_wrap(~as.factor(y), nrow=1) + 
  theme_bw() + 
  theme(panel.grid=element_blank()) + 
  labs(x="First Difference\n(2 SD Change)", y="")
ggsave("output/f11_8.png", height=4, width=10, units="in", dpi=300)

