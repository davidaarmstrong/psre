## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)

## load data from psre package
data(wvs)

## filter to only waves 2 and 3 of the data. 
## turn democrat into a factor with the appropriate labels
## keep only the first instance where the country appears in the data
wvs0 <- wvs %>% 
  filter(wave %in% c(3,2)) %>% 
  mutate(democrat= factor(democrat, levels=1:2, 
                          labels=c("New Democracy", "Established Democracy"))) %>%
  group_by(country) %>% 
  arrange(wave) %>% 
  slice_head(n=1) %>% 
  select(secpay, gini_disp, democrat) %>% 
  na.omit()

## linear additive model 
lmod <- lm(secpay ~ gini_disp + democrat, data=wvs0)

## save residuals and fitted values into the dataset
wvs0$el <- rstudent(lmod)
wvs0$fitl <- fitted(lmod)

## A. Model 7a: Linear
ggplot(wvs0, aes(x=fitl, y=el)) + 
  geom_point(shape=1) + 
  theme_classic() + 
  labs(x="Fitted Values", 
       y="Studentized Residuals")
# ggssave("output/f7_10a.png", height=4.5, width=4.5, units="in", dpi=300)


## estimate interaction model
imod <- lm(secpay ~ gini_disp*democrat, data=wvs0)
## save residuals and fitted values into data.
wvs0$ep <- rstudent(imod)
wvs0$fitp <- fitted(imod)

## B. Model 7e: Interaction 
ggplot(wvs0, aes(x=fitp, y=ep)) + 
  geom_point(shape=1) + 
  theme_classic() + 
  labs(x="Fitted Values", 
       y="Studentized Residuals")
# ggssave("output/f7_10b.png", height=4.5, width=4.5, units="in", dpi=300)


## The code below does not produce a figure or table 
## that is in the text, but we discuss briefly that
## methods for fixing heteroskedasticity had little effect.
## Here are the methods that we tried. 

library(lmtest)
library(hcci)
library(sandwich)
## generate p-values using different types of 
## heteroskedasticity robust standard errors
coeftest(imod, vcov=vcovHC, type="HC0")
coeftest(imod, vcov=vcovHC, type="HC1")
coeftest(imod, vcov=vcovHC, type="HC2")
coeftest(imod, vcov=vcovHC, type="HC3")
coeftest(imod, vcov=vcovHC, type="HC4")
coeftest(imod, vcov=vcovHC, type="HC4m")
coeftest(imod, vcov=vcovHC, type="HC5")

## make the second and third order regressors in the data
wvs0$g2 <- wvs0$gini_disp^2
wvs0$g3 <- wvs0$gini_disp^3
## make the democratic history dummy regressor in the data
wvs0$demdum <- as.numeric(wvs0$democrat == "Established Democracy")

## Put secpay, gini, democratic history and the interaction 
## in the workspace (i.e., directly in R's search path)
sp <- wvs0$secpay
g <- wvs0$gini_disp
d <- as.numeric(wvs0$democrat == "Established Democracy")
gd <- g*d

## Wild Bootstrap
## the wild bootstrap requires that the variables in the model
## be directly in R's search path. 
im <- lm(sp ~ g + d + gd )
Pboot(im)

## heteroskedastic regression 
source("https://quantoid.net/files/r/MLhetReg.R")
## estimate the model you want to use
imod <- lm(secpay ~ gini_disp*democrat, data=wvs0)
## make the design matrix from the model, removing the intercept
X <- model.matrix(imod)[,-1]
## replicate X as Z - Z is the set of variables that 
## will predict the residual variance
Z <- X
## estimate the heteroskedastic regression model
het.mod <- MLhetreg(wvs0$secpay, X, Z)
## summarise the model 
summary(het.mod)


## Feasible Generalized Least Squares (FGLS)
## estimate the auxiliary regression of the log of 
## the squared residuals on the RHS variables
aux.mod1 <- lm(log(resid(imod)^2) ~ gini_disp*democrat, data=wvs0)
## get the predicted log-residual variance and exponentiate it
h <- exp(predict(aux.mod1))
## use 1/h as the weights in a weighted least squares model.
mod.fgls <- lm(secpay ~ gini_disp*democrat, data=wvs0, weights=1/h)
## summarise. 
with(summary(mod.fgls), coefficients)

