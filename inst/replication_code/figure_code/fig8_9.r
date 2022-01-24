## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(ggeffects)

## load data from psre package
data(repress)

## repalce pr = NA if pr < 0
repress$pr <- ifelse(repress$pr < 0, NA, repress$pr)
## divide pr by 100
repress$pr <- repress$pr/100
## make regressors for log of gdp and log of population 
## select the required variables and then listwise delete
repress <- repress %>% mutate(log_gdp = log(rgdpe), 
                              logpop = log(pop)) %>% 
  dplyr::select(pr, cwar, iwar, rgdpe, pop, log_gdp, logpop, pts_s) %>% 
  na.omit()

## estimate loess model with span = .25
lo <- loess(pts_s ~ pr, data=repress, span=.25)

## generate data  where pr varies across its range. 
fake <- data.frame(pr = seq(0,.4, length=100))
## generate predictions from the loess model
f1 <- predict(lo, newdata=fake, se=TRUE)
## make confidence intervals and a variable indicating the 
## span used to make the predictions 
fake1 <- fake %>% mutate(fit = f1$fit, low=fit - 1.96*f1$se.fit, 
                         up = fit + 1.96*f1$se.fit, 
                         span = factor(1, levels=1:2, labels=c("0.25", "0.5")))
## proceed as above, but using span = .5 instead
lo <- loess(pts_s ~ pr, data=repress, span=.5)
f1 <- predict(lo, newdata=fake, se=TRUE)
fake3 <- fake %>% mutate(fit = f1$fit, low=NA, 
                         up = NA, 
                         span = factor(2, levels=1:2, labels=c("0.25", "0.5")))

lofit <- bind_rows(fake1, fake3)


## A. Local Polynomial Regression
ggplot(lofit, aes(x=pr, y=fit, ymin = low, ymax=up, linetype=span)) + 
  geom_ribbon(alpha=.2, show.legend=FALSE) + 
  geom_line() + 
  theme_classic() + 
  theme(legend.position=c(.15,.15)) + 
  labs(x="Political Rights", y="Predicted Repression")
# ggssave("output/f8_9a.png", height=4.5, width=4.5, units="in", dpi=300)

## estimate the smoothing spline
ss <- smooth.spline(repress$pr, repress$pts_s, all.knots=TRUE, cv=TRUE)

## make a function that we can use to get bootstrap estimates 
## of the smoothing spline to produce a confidence interval
bss <- function(data, inds){
  tmp <- data[inds, ]
  m <- smooth.spline(tmp$pr, tmp$pts_s, spar=ss$spar)
  predict(m, x=fake$pr)$y
}

library(boot)
## bootstrap the smoothing spline
bootss <- boot(repress, bss, R=2500)
## make confidence intervals and add them to 
## the prediction data. 
fakess <- fake %>% 
  mutate(fit = predict(ss, x=fake$pr)$y, 
         low = apply(bootss$t, 2, quantile, .025), 
         up = apply(bootss$t, 2, quantile, .975))

## B. Smoothing Spline
ggplot(fakess, aes(x=pr, y=fit, ymin=low, ymax=up)) +
  geom_ribbon(alpha=.2) + 
  geom_line() + 
  theme_classic() + 
  labs(x="Political Rights", y="Predicted Repression")
# ggssave("output/f8_9b.png", height=4.5, width=4.5, units="in", dpi=300)





