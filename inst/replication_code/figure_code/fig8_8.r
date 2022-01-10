## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(splines)
library(rsample)
library(ggeffects)

## load data from psre package
data(repress)

## make left- and right- basis functions for the linear 
## piecewise regression model 
BL <- function(x,c=.34)ifelse(x < c, c-x, 0)
BR <- function(x,c=.34)ifelse(x > c, x-c, 0)

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

## estimate regression models - b-spline, natural spline and piecewise linear regression
mbs <- lm(pts_s ~ bs(pr, df = 12)+ cwar + iwar + log_gdp + logpop, data=repress)
mns <- lm(pts_s ~ ns(pr, df = 12)+ cwar + iwar + log_gdp + logpop, data=repress)
mpwl <- lm(pts_s ~ BL(pr, .34) + BR(pr, .34) + cwar + iwar + log_gdp + logpop, data=repress)
## Generate predicted values from each model
epwlm <- ggpredict(mpwl, "pr [all]")
ensm <- ggpredict(mns, "pr [all]")
ebsm <- ggpredict(mbs, "pr [all]")

## Add into each set of predictions a variable that identifies
## from which model the prediction comes. 
epwlm <- epwlm %>% mutate(model = factor(1, levels=1:3, labels=c("Piecewise Linear", "Nat. Spline", "B-spline")))
ebsm <- ebsm %>% mutate(model = factor(2, levels=1:3, labels=c("Piecewise Linear", "Nat. Spline", "B-spline")))
ensm <- ensm %>% mutate(model = factor(3, levels=1:3, labels=c("Piecewise Linear", "Nat. Spline", "B-spline")))

## Put all prediction data together
epb <- bind_rows(epwlm, ensm, ebsm)

## Filter out natural spline data
epb2 <- epb %>% filter(model != "Nat. Spline") 

## A. B-spline and Piecewise Linear Model
ggplot() + 
  geom_ribbon(data=epb2, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high, group=model), alpha=.2) + 
  geom_line(data=epb2, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high, group=model, linetype=model)) + 
  theme_classic() +
  theme(legend.position=c(.8, .9),
        legend.text = element_text(size=10)) + 
  labs(x="Political Rights", y="Predicted Repression", 
       linetype="")
ggsave("output/f8_8a.png", height=4.5, width=4.5, units="in", dpi=300)

## Filter out piecewise linear model 
epb %>% filter(model != "Piecewise Linear") %>% 
  ## B. B-spline and natural spline
  ggplot(aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high, group=model)) + 
  #  geom_ribbon(alpha=.2) + 
  geom_line(aes(linetype=model)) + 
  theme_classic() +
  theme(legend.position=c(.8, .9),
        legend.text = element_text(size=10)) + 
  labs(x="Political Rights", y="Predicted Repression", 
       linetype="")
ggsave("output/f8_8b.png", height=4.5, width=4.5, units="in", dpi=300)

#################

## While we don't present these results in the book directly, 
## below is how we find which model is best. 

## Choosing the right Spline Parameterization
## Make formulas for bs models with varying different degrees of freedom.  
bs_forms <- list()
bs_forms[[1]] <- pts_s ~ pr + cwar + iwar + log_gdp + logpop
bs_forms[[2]] <- pts_s ~ poly(pr, 2) + cwar + iwar + log_gdp + logpop
bs_forms[[3]] <- pts_s ~ poly(pr, 3) + cwar + iwar + log_gdp + logpop
bs_forms[[4]] <- pts_s ~ bs(pr, df = 4)+ cwar + iwar + log_gdp + logpop
bs_forms[[5]] <- pts_s ~ bs(pr, df = 5)+ cwar + iwar + log_gdp + logpop
bs_forms[[6]] <- pts_s ~ bs(pr, df = 6)+ cwar + iwar + log_gdp + logpop
bs_forms[[7]] <- pts_s ~ bs(pr, df = 7)+ cwar + iwar + log_gdp + logpop
bs_forms[[8]] <- pts_s ~ bs(pr, df = 8)+ cwar + iwar + log_gdp + logpop
bs_forms[[9]] <- pts_s ~ bs(pr, df = 9)+ cwar + iwar + log_gdp + logpop
bs_forms[[10]] <- pts_s ~ bs(pr, df = 10)+ cwar + iwar + log_gdp + logpop
bs_forms[[11]] <- pts_s ~ bs(pr, df = 11)+ cwar + iwar + log_gdp + logpop
bs_forms[[12]] <- pts_s ~ bs(pr, df = 12)+ cwar + iwar + log_gdp + logpop
bs_forms[[13]] <- pts_s ~ bs(pr, df = 13)+ cwar + iwar + log_gdp + logpop

## Make formulas for different ns models with different degrees of freedom
ns_forms <- list()
ns_forms[[1]] <- pts_s ~ pr + cwar + iwar + log_gdp + logpop
ns_forms[[2]] <- pts_s ~ poly(pr, 2) + cwar + iwar + log_gdp + logpop
ns_forms[[3]] <- pts_s ~ poly(pr, 3) + cwar + iwar + log_gdp + logpop
ns_forms[[4]] <- pts_s ~ ns(pr, df = 2)+ cwar + iwar + log_gdp + logpop
ns_forms[[5]] <- pts_s ~ ns(pr, df = 3)+ cwar + iwar + log_gdp + logpop
ns_forms[[6]] <- pts_s ~ ns(pr, df = 4)+ cwar + iwar + log_gdp + logpop
ns_forms[[7]] <- pts_s ~ ns(pr, df = 5)+ cwar + iwar + log_gdp + logpop
ns_forms[[8]] <- pts_s ~ ns(pr, df = 6)+ cwar + iwar + log_gdp + logpop
ns_forms[[9]] <- pts_s ~ ns(pr, df = 7)+ cwar + iwar + log_gdp + logpop
ns_forms[[10]] <- pts_s ~ ns(pr, df = 8)+ cwar + iwar + log_gdp + logpop
ns_forms[[11]] <- pts_s ~ ns(pr, df = 9)+ cwar + iwar + log_gdp + logpop
ns_forms[[12]] <- pts_s ~ ns(pr, df = 10)+ cwar + iwar + log_gdp + logpop
ns_forms[[13]] <- pts_s ~ ns(pr, df = 11)+ cwar + iwar + log_gdp + logpop

## Estimate the bs and ns models
bs_mods <- lapply(bs_forms, function(x)lm(x, data=repress))
ns_mods <- lapply(ns_forms, function(x)lm(x, data=repress))
## get the AIC and BIC both sets of models 
bs_a <- sapply(bs_mods, AIC)
ns_a <- sapply(ns_mods, AIC)
bs_b <- sapply(bs_mods, BIC)
ns_b <- sapply(ns_mods, BIC)

## Find the models with the smallest AIC and BIC 
which.min(ns_a)
which.min(bs_a)
which.min(ns_b)
which.min(bs_b)

## get the knot locations for the bs and ns models estimated above
bsk <- lapply(bs_mods, function(x)attr(model.frame(x)[,2], "knots"))
nsk <- lapply(ns_mods, function(x)attr(model.frame(x)[,2], "knots"))

## make a function that we'll use in the cross-validation run 
cv_mods <- function(split, ...){
  ## initialize lists to hold the results
  bsm <- nsm <- list()
  k <- 1
  ## loop over degrees of freedom estimating models using 
  ## the analysis (training) data
  for(j in 4:13){
    bsm[[k]] <- lm(pts_s ~ bs(pr, knots=bsk[[j]])+ cwar + iwar + log_gdp + logpop, data=analysis(split))
    nsm[[k]] <- lm(pts_s ~ ns(pr, knots=nsk[[j]])+ cwar + iwar + log_gdp + logpop, data=analysis(split))
    k <- k+1 
  }
  ## make a vector of y values from the testing data 
  y <- assessment(split)$pts_s
  ## generate predictions for all of the models using the testing data
  bsf <- lapply(bsm, function(x)predict(x, newdata=assessment(split)))
  nsf <- lapply(nsm, function(x)predict(x, newdata=assessment(split)))
  
  ## calculate the sum of squared errors for the training data
  bsse <- sapply(bsf, function(x)sum((y-x)^2))
  nsse <- sapply(nsf, function(x)sum((y-x)^2))
  # make a list that contains all of the results
  tmp <- as.list(c(bsse, nsse))
  names(tmp) <- c(paste0("bs", 4:13), paste0("ns", 4:13))
  ## turn the result in to a data frame
  do.call(data.frame, tmp)
}

## estimate the cross-validation 
out <- vfold_cv(repress,
                v=10,
                repeats = 10) %>%
  mutate(err = map(splits,
                   cv_mods)) 
## turn the results into a data frame that 
## can easily be used to identify the 
## best model. 
out2 <- out %>% 
  unnest(err)  %>% 
  group_by(id) %>% 
  summarise(across(bs4:ns13, ~sum(.x)))
## calculate the mean cross-validation error for each 
## iteration of the cross-validation. 
outsum <- out2 %>% 
  summarise(across(bs4:ns13, ~mean(.x)))

## calculate difference between the cross-validation error and the
## minimum cross-validation error. 
outsum %>% 
  pivot_longer(everything(), names_pattern="([bn]s)(\\d+)", names_to=c(".value", "df")) %>% 
  mutate(across(c(bs, ns), ~round(.x-min(.x), 1)))

