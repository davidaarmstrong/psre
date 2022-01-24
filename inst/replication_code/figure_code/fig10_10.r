## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(rio)
library(statmod)

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

## Make data frame that has original data,
## systematic component related to the 
## independent variables and the model 
## standardized deviance residuals. 
edat <- data.frame(
  age = gss$age, 
  educ = gss$educ,
  sei01 = gss$sei01, 
  com_age = predict(moda, type="terms")[, "age"], 
  com_educ = predict(moda, type="terms")[, "educ"],
  com_sei = predict(moda, type="terms")[, "sei01"],
  res = rstandard(moda, type="deviance"))

## A. Age
ggplot(edat, aes(x=age, y=(com_age + res))) + 
  geom_point(shape=1, col="gray50") + 
  geom_smooth(aes(linetype = "Linear"), method="lm", se=FALSE, col="black") + 
  geom_smooth(aes(linetype = "GAM"), method="gam", formula =y ~ s(x, k=5), 
              se=FALSE, col="black") + 
  theme_classic() + 
  theme(legend.position = "top") + 
  labs(x="Age", y="Component + Quantile Residual", linetype = "")
# ggssave("output/f10_10a.png", height=4.5, width=4.5, units="in", dpi=300)

## B. Education
ggplot(edat, aes(x=educ, y=(com_educ + res))) + 
  geom_point(shape=1, col="gray50", position=position_jitter(width=.15)) + 
  geom_smooth(aes(linetype = "Linear"), method="lm", se=FALSE, col="black") + 
  geom_smooth(aes(linetype = "GAM"), method="gam", formula =y ~ s(x, k=5), 
              se=FALSE, col="black") + 
  theme_classic() + 
  theme(legend.position = "top") + 
  labs(x="Education", y="Component + Quantile Residual", linetype = "")
# ggssave("output/f10_10b.png", height=4.5, width=4.5, units="in", dpi=300)

## C. Socioeconomic status
ggplot(edat, aes(x=sei01, y=(com_sei + res))) + 
  geom_point(shape=1, col="gray50", position=position_jitter(width=.15)) + 
  geom_smooth(aes(linetype = "Linear"), method="lm", se=FALSE, col="black") + 
  geom_smooth(aes(linetype = "GAM"), method="gam", formula =y ~ s(x, k=5), 
              se=FALSE, col="black") + 
  theme_classic() + 
  theme(legend.position = "top") + 
  labs(x="Socio-economic Status", y="Component + Quantile Residual", linetype = "")
# ggssave("output/f10_10c.png", height=4.5, width=4.5, units="in", dpi=300)
