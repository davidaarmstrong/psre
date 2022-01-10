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

## get the standardized deviance residuals 
## and the quantile residuals
resids <- tibble(
  c(qresid(moda), rstandard(moda, type="deviance")), 
  type = factor(rep(1:2, each=nobs(moda)), labels=c("Quantile", "Std. Deviance"))
)

## set the random number generating seed
set.seed(519)
## get the randomized quantile residuals
## and take the median over 100 draws
qres <- sapply(1:100, function(i)qresid(moda))
qr <- apply(qres, 1, median)

## A. Randomized quantile residuals
ggplot() + 
  geom_histogram(aes(x=qr, y=..density..), col="white") + 
  theme_classic() + 
  geom_function(fun = "dnorm", size=1) + 
  labs(x="Quantile Residuals")
ggsave("output/f10_6a.png", height=4.5, width=4.5, units="in", dpi=300)

## B. Standardized deviance residuals
ggplot() + 
  geom_histogram(aes(x=rstandard(moda, type="deviance"), y=..density..), col="white") + 
  theme_classic() + 
  geom_function(fun = "dnorm", size=1) + 
  labs(x="Quantile Residuals")
ggsave("output/f10_6b.png", height=4.5, width=4.5, units="in", dpi=300)
