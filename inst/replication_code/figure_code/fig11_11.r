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
## same ordered logit model, but using polr from MASS
opwlm <- MASS::polr(pts_fac ~ BL(pr, .34) + BR(pr, .34) + cwar + iwar + log(rgdpe) + log(pop), data=repress)
## linear model 
linpwl <- lm(pts_s ~ BL(pr, .34) + BR(pr, .34) + cwar + iwar + log(rgdpe) + log(pop), data=repress)

## Correlation of Xb
## make design matrix for ordinal model
X <- model.matrix(opwlm)[,-1]
## get coefficients for ordinal model 
b <- coef(opwlm)
## create Xb for ordinal model
Xb <- X %*% b
## correlate ordered logit xb with lm xb
cor(Xb, fitted(linpwl))


## Expected Value from ORM
## Predict probabilities for owplm
mprob <- predict(opwlm, type="probs")
## The expected value of y is the probabilities
## of each outcome multiplied by the value of each
## outcome
Ey <- mprob %*% 1:5

## correalte Ey with the fitted values from 
## the linear model 
cor(Ey, fitted(linpwl))

## make a sequence of values from 0 to .4, the
## range of pr
s <- seq(0,.4, by=.01)

## initialize two containers for results
fit1 <- fit2 <- NULL
## loop over the hypothetical values of pr
for(i in 1:length(s)){
  ## get the data to estimate the model
  tmp <- get_all_vars(formula(opwlm), repress) %>% na.omit()
  ## set pr to the ith value of s
  tmp$pr <- s[i]
  ## generate expected value for this particular 
  ## value of s
  e1 <- predict(opwlm, newdata=tmp, type="prob")
  e1 <- c(e1 %*% 1:5)
  ## get predicted probabilities from the linear
  ## model for this value of s
  e2 <- predict(linpwl, newdata=tmp)
  ## save the results
  fit1 <- rbind(fit1, e1)
  fit2 <- rbind(fit2, e2)
}

## calculate the difference between the 
## two different measures of fit
dif <- abs(fit1-fit2)
## calculate the standard deviation 
## of the difference for each value of s
dsd <- apply(dif, 2, sd)
## what's the biggest standard deviation?
dsd[which.max(dsd)]

## Consider observation 1833
i <- 1833
tmp <- data.frame(
  e1 = fit1[,i], 
  e2= fit2[,i], 
  x = s
)

## make plot for obs 1833's values of the 
## variables aside from pr. 
ggplot(tmp) + 
  geom_line(aes(x=x, y=e1, linetype="ORM")) + 
  geom_line(aes(x=x, y=e2, linetype="OLS")) + 
  theme_classic() + 
  theme(legend.position=c(.15, .15)) + 
  labs(x="Political Rights", y="Predicted State Repression", 
       linetype="")
# ggssave("output/f11_11.png", height=4.5, width=4.5, units="in", dpi=300)



