## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages

library(tidyverse)
library(psre)
library(brant)
library(DAMisc)

## load data from psre package
data(repress)
## replace pr = NA if pr < 0
repress$pr <- ifelse(repress$pr < 0, NA, repress$pr)
## rescale pr 
repress$pr <- repress$pr/100

## generate variables and convert to factor
repress <- repress %>% mutate(log_gdp = log(rgdpe), 
                              logpop = log(pop),
                              pts_fac = as.factor(pts_s), 
                              cwar = as.factor(cwar), 
                              iwar = as.factor(iwar)) %>% 
  dplyr::select(pr, cwar, iwar, rgdpe, pop, log_gdp, logpop, pts_s, pts_fac) %>% 
  na.omit()

## make basis functions
BL <- function(x,c=.34)ifelse(x < c, c-x, 0)
BR <- function(x,c=.34)ifelse(x > c, x-c, 0)

## estimate ordered logit model

repress$bl <- BL(repress$pr, .34)
repress$br <- BR(repress$pr, .34)

opwl <- MASS::polr(pts_fac ~ bl + br + cwar + iwar + log_gdp + logpop, data=repress)

## calculate brant test
brant(opwl)

## estimate multinomial logit model
mrm <-nnet::multinom(pts_fac ~ BL(pr, .34) + BR(pr, .34) + cwar + iwar +  log(rgdpe) + log(pop), data=repress)

## Make table 
b <- coef(mrm)
v <- vcov(mrm)
b <- c(t(b))
se <- sqrt(diag(v))
pv <- 2*pnorm(abs(b/se), lower.tail=FALSE)
tab11_7 <- matrix(shuffle(b, pv, se), ncol=4)
rownames(tab11_7) <- rep("", 14)
rownames(tab11_7)[seq(1, 14, by=2)] <- colnames(coef(mrm))
colnames(tab11_7) <- paste0("PTS = ", 2:5)
noquote(tab11_7)

## model fit statistics
pre(mrm, sim=TRUE)
logLik(mrm)
AIC(mrm)
BIC(mrm)

