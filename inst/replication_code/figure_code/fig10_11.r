## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(rio)
library(statmod)
library(splines)

## load data from psre package
data(gss)

## decrease the variance of sei01, make sex a factor, 
## and recode values of education less than 6 to 6. 
## select only the required variables, then listwise
## delete. 
gss <- gss %>% 
  mutate(sei01 = sei10/100, 
         sex = factorize(sex),
         educ = case_when(
           educ < 6 ~ 6, 
           TRUE ~ educ)) %>% 
  dplyr::select(childs, age, sei01, sex, educ) %>% 
  na.omit()

## estimate a poisson glm
moda <- glm(childs ~ sei01 + sex + educ + age, 
            data=gss, family=poisson)

## estimate fourth-degree polynomial in age
polymod <- glm(childs ~ poly(age, 4) + sei01 + sex + educ,
                  data=gss, family=poisson)

## estimate quadratic spline with two knots for age
splmod <- glm(childs ~ bs(age, degree=2, knots=c(43.5, 80.5)) + sei01 + sex + educ,
                  data=gss, family=poisson)

## create average marginal effect plot data
## for age from the polynomial and spline 
## models above. 
e1 <- aveEffPlot(polymod, "age", gss, nvals = 50, plot=FALSE, return="ci")
e2 <- aveEffPlot(splmod, "age", gss, nvals = 50, plot=FALSE, return="ci")

## keep the relevant data from e1 and e2
e1 <- e1$ci %>% as.data.frame %>% 
  setNames(c("x", "predicted", "conf.low", "conf.high"))
e2 <- e2$ci %>% as.data.frame %>% 
  setNames(c("x", "predicted", "conf.low", "conf.high"))

## make model indicator variables in e1 and e2
e1$model <- factor(1, levels=1:2, labels=c("Polynomial", "Spline"))
e2$model <- factor(2, levels=1:2, labels=c("Polynomial", "Spline"))

## put effect data together. 
e <- bind_rows(e1, e2)

## make plot
ggplot(e, aes(x=x, y=predicted, 
              ymin = conf.low, ymax=conf.high, 
              group=model)) + 
  geom_ribbon(alpha=.2, colour="transparent") + 
  geom_line(aes(linetype=model)) + 
  theme_classic() + 
  theme(legend.position = c(0.2, 0.95)) + 
  labs(x="Age", y="Expected # Children", linetype="")
# ggssave("output/f10_11.png", height=4.5, width=4.5, units="in", dpi=300)
