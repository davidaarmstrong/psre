## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(DAMisc)
library(rio)

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

## use glmChange2 from the DAMisc package
## to get the average marginal effect
g2 <- glmChange2(moda, gss, varname = "age", diffchange="unit", n=5, sim=TRUE)

## make plot
ggplot() + 
  geom_histogram(aes(x=g2$avesamp), col="white") +
  geom_vline(xintercept=mean(g2$avesamp), linetype=2, size=1.25) + 
  theme_classic() +
  labs(x="First Differences for Age\n(+/- 2.5 Years)", y="Frequency")
ggsave("output/f10_2.png", height=4.5, width=4.5, units="in", dpi=300)
