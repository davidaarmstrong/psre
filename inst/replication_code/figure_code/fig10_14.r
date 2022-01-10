## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(rio)
library(splines)
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

## estimate a poisson glm with spline
splmod <- glm(childs ~ bs(age, degree=2, knots=c(43.5, 80.5)) + sei01 + sex + educ,
                  data=gss, family=poisson)

## get fitted values, residuals and 
## constant information scale from model
res_datb <- tibble(
  fit = fitted(splmod), 
  eq = qresid(splmod), 
  cis_fit = sqrt(fit))

## A. Residual plot
ggplot(res_datb, 
       aes(x=cis_fit, y=eq)) + 
  geom_point(shape=1, col="gray50") + 
  geom_smooth(method="loess", se=FALSE, col="black") + 
  theme_classic() + 
  labs(x="Constant Information Scale", y="Standardized Deviance Residuals")
ggsave("output/f10_14a.png", height=4.5, width=4.5, units="in", dpi=300)


## get quantile-quantile data
qqdf <- qqPoints(qresid(splmod))
a <- attr(qqdf, "ab")[1]
b <- attr(qqdf, "ab")[2]
l <- min(qqdf$theo) * b + a
u <- max(qqdf$theo) * b + a

## B. Quantile Comparison Plot
ggplot(qqdf, aes(x=theo, y=x)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15) + 
  geom_segment(aes(x=min(qqdf$theo), xend=max(qqdf$theo), y = l, yend=u)) + 
  geom_point(shape=1) + 
  theme_classic() + 
  labs(x="Theoretical Quantiles", 
       y="Observed Quantiles")
ggsave("output/f10_14fb.png", height=4.5, width=4.5, units="in", dpi=300)

