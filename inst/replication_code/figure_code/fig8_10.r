## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(ggeffects)
library(gamlss)
library(mgcv)
library(boot)

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

## estimate the GAM with the gamlss function 
g1 <- gamlss(pts_s ~ pb(pr) + cwar + iwar + log_gdp + logpop, data=repress)

## make a hypothetical dataset that will be used to generate predictions. 
fakeX <- expand.grid(
  pts_s = 2,
  cwar = 0, 
  iwar = 0, 
  log_gdp = log(median(repress$rgdpe)), 
  logpop = log(median(repress$pop)), 
  pr = seq(0, .4, length=41)
)

## bootstrap predictions from the model
gbres <- NULL
sink(tempfile())
for(i in 1:100){
  tmp <- repress[sample(1:nrow(repress), nrow(repress), replace=TRUE), ]
  g <- gamlss(pts_s ~ pb(pr, lambda = 33.2278) + cwar + iwar + log_gdp + logpop, data=tmp)
  gbres <- rbind(gbres, predict(g, newdata=fakeX))
}
sink()

## calculate normal-theory confidence intervals for the 
## bootstrapped predictions
fakeX$fit <- predict(g1, newdata=fakeX)
fakeX$low <- fakeX$fit - 1.96*apply(gbres, 2, sd)
fakeX$up <- fakeX$fit + 1.96*apply(gbres, 2, sd)



## Grid search for span in loess smoother
## identify points of span that will be used in the 
## loess smoother. 
span_seq <- seq(.1, .95, by=.01)

## estimate the models for every different span
sink(tempfile())
g2mods <- lapply(span_seq, function(s)gamlss(pts_s ~ lo(~pr, span=s) + 
                                               cwar + iwar + log_gdp + logpop, 
                                             data=repress))
sink()
## get the AIC value for each model 
aics <- sapply(g2mods, function(x)x$aic)

## Find the span with the smallest AIC
span_seq[which.min(aics)]

## estimate the model with the optimized span parameter
g2 <- gamlss(pts_s ~ lo(~pr, span=.15) + cwar + iwar + log_gdp + logpop, data=repress)

## generate some hypthetical data that will be used 
## to make predictions
fakeX2 <- expand.grid(
  cwar = 0, 
  iwar = 0, 
  log_gdp = log(median(repress$rgdpe)), 
  logpop = log(median(repress$pop)), 
  pr = seq(0, .4, length=41), 
  pts_s = 2
)

gbres2 <- NULL
sink(tempfile())
for(i in 1:100){
  tmp <- repress[sample(1:nrow(repress), nrow(repress), replace=TRUE), ]
  g <- gamlss(pts_s ~ lo(~pr, span=.15) + cwar + iwar + log_gdp + logpop, data=tmp)
  gbres2 <- rbind(gbres2, predict(g, newdata=fakeX2))
}
sink()

fakeX2$fit <- predict(g2, newdata=fakeX2)
fakeX2$low <- fakeX2$fit - 1.96*apply(gbres2, 2, sd)
fakeX2$up <- fakeX2$fit + 1.96*apply(gbres2, 2, sd)

## Make piecewise linear model for comparison 
BL <- function(x,c=.34)ifelse(x < c, c-x, 0)
BR <- function(x,c=.34)ifelse(x > c, x-c, 0)
pwlm <- lm(pts_s ~ BL(pr, .34) + BR(pr, .34) + cwar + iwar + log_gdp + logpop, data=repress)
## generate effects for piecewise linear model
epwlm <- ggpredict(pwlm, terms="pr [all]")

## keep only the necessary variables from the fakeX data
fakeXs <- fakeX %>% dplyr::select(pr, fit, low, up) %>% 
  mutate(model = factor(2, levels=1:2, labels=c("Piecewise Linear", "Smoothing Spline")))
## keep only the necessary variables from the epwlm data
## change names to match names in the fakeX data
epwlm <- epwlm %>% 
  dplyr::select(x, predicted, conf.low, conf.high) %>%
  setNames(c("pr", "fit", "low", "up")) %>%
  mutate(model = factor(1, levels=1:2, labels=c("Piecewise Linear", "Smoothing Spline"))) %>% 
  as.data.frame()

## put epwlm and fakeXs data together
ss_fit <- bind_rows(fakeXs, epwlm)


## A. Smoothing Spline
ggplot(ss_fit, aes(x=pr, y=fit, ymin=low, ymax=up, group=model)) + 
  geom_ribbon(alpha=.2) + 
  geom_line(aes(linetype=model)) + 
  theme_classic() + 
  theme(legend.position=c(.2,.15)) + 
  labs(x="Political Rights", y="Predicted Repression", linetype="")
# ggssave("output/f8_10a.png", height=4.5, width=4.5, units="in", dpi=300)

## Proceed as above, but with the fakeX2 data
fakeX2s <- fakeX2 %>% dplyr::select(pr, fit, low, up) %>% 
  mutate(model = factor(2, levels=1:2, labels=c("Piecewise Linear", "Local Polynomial")))

epwlm <- epwlm %>% 
  mutate(model = factor(1, levels=1:2, labels=c("Piecewise Linear", "Local Polynomial"))) %>% 
  as.data.frame()

lo_fit <- bind_rows(fakeX2s, epwlm)

## B. Local polynomial
ggplot(lo_fit, aes(x=pr, y=fit, ymin=low, ymax=up, group=model)) + 
  geom_ribbon(alpha=.2) + 
  geom_line(aes(linetype=model)) + 
  theme_classic() + 
  theme(legend.position=c(.2,.15)) + 
  labs(x="Political Rights", y="Predicted Repression", linetype="")
# ggssave("output/f8_10b.png", height=4.5, width=4.5, units="in", dpi=300)
