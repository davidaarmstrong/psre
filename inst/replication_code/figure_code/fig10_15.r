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

## get dfbetas
dfb <- dfbetas(splmod)

## calculate the row-means of the dfbetas
dfb2 <- rowMeans(dfb^2)

## make a data frame with the dfbetas results
tmp <- data.frame(all = dfb2, obs = 1:length(dfb2))

## create influence data with studentized
## deviance residuals, hat values and 
## cook's distance
infl.df <- tibble(
  rstu = rstudent(splmod, type="deviance"),
  hat = hatvalues(splmod),
  d = cooks.distance(splmod))

## add influence data to dfbetas data
infl.df <- bind_cols(tmp, infl.df)

## generate cut-points for the leverage
hat_cut <- 3*length(splmod$coef)/nobs(splmod)

## A. Influence plot
ggplot(infl.df, aes(x=hat, y=rstu, size=d)) + 
  geom_point(shape=1, show.legend = FALSE) + 
  theme_classic() + 
  geom_hline(yintercept=c(-2,2), lty=2) + 
  geom_hline(yintercept=0, lty=3) + 
  geom_vline(xintercept=hat_cut, lty=2) + 
  labs(x="Hat Values", y="Studentized Deviance Residuals")
ggsave("output/f10_15a.png", height=4.5, width=4.5, units="in", dpi=300)

## B. DFBETA plot
dfbhist(tmp, "all", "obs", .0075, binwidth=.0001, 
        nudge_y=c(100,-100,100,100,-100,150), 
        nudge_x=c(0, -.001,0,0,.002,0))
ggsave("output/f10_15b.png", height=4.5, width=4.5, units="in", dpi=300)
