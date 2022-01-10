## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(stargazer)

## load data from psre package
data(wvs)

## pick first instance of country in wave 2 or 3
## remove czech republic and slovakia
## make democratic history factor
tmp <- wvs %>% 
  select(secpay, gini_disp, democrat, country, wave) %>% 
  filter(wave %in% 2:3) %>% 
  group_by(country) %>% 
  arrange(wave) %>% 
  slice_head(n=1) %>% 
  na.omit() %>% 
  ungroup %>% 
  filter(!(country %in% c("Czech Rep.", "Slovakia"))) %>% 
  mutate(democrat = factor(democrat, labels=c("New Democracy", "Established Democracy")))

## make left and right piecewise linear basis functions
BL <- function(x,c=.34)ifelse(x < c, c-x, 0)
BR <- function(x,c=.34)ifelse(x > c, x-c, 0)

## While what comes directly below isn't reported in the 
## table, it is how we picked the right knot value 
## for the piecewise linear spline. 

## get dependent variable
y <- model.response(model.frame(secpay ~ gini_disp + democrat, data=tmp))
## use values between .248 and .467 as the possible knot 
## locations.  These values are the 10th and 90th 
## percentile values of gini_disp
kseq <- seq(.248, .467, length=100)

## initialize container for results
ll <- NULL
## loop over possible knot locations
for(i in 1:length(kseq)){
  ## estimate model with desired knot location
  m <- lm(y ~ BL(gini_disp, kseq[i]) + BR(gini_disp, kseq[i]) + democrat, data=tmp)
  ## save log-likelihood
  ll <- c(ll, logLik(m))
}
## find which knot location did best
round(kseq[which.max(ll)], 2)

## estimate piecewise linear model
ms <- lm(secpay ~ BL(gini_disp) + BR(gini_disp) + democrat, data=tmp)
## estimate linear interaction model
mi <- lm(secpay ~ gini_disp*democrat, data=tmp)
## estimate quadratic polynomial model
mp <- lm(secpay ~ gini_disp + I(gini_disp^2) + democrat, data=tmp)

## calculate AIC, BIC and deltas
a <- AIC(mp, mi, ms)
b <- BIC(mp, mi, ms)
da <- round(a-min(a), 1)[,2]
db <- round(b-min(b), 1)[,2]

## get the adjusted r-squared
ar2 <- c(
  summary(mp)$adj.r.squared, 
  summary(mi)$adj.r.squared, 
  summary(ms)$adj.r.squared
)

## Make the table of fit statistics
st <- rbind(ar2, da, db)
colnames(st) <- c("M8d", "M8c", "M8e")
st <- st[,c(2,1,3)]
stats <- bind_cols(tibble(measure = c("Adj. R-squared", "Delta AIC", "Delta_BIC")), 
               as_tibble(st))

## print the table of models 
stargazer(mi, mp, ms, digits=2, type="text")
## print the table of fit statistics
stats



