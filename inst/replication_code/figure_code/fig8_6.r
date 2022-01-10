## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(ggeffects)

## load data from psre package
data(wvs)

## omit missing values on gini_disp
## poly() will not accept missing data. 
wvs <- wvs %>% filter(!is.na(gini_disp))

## choose required variables, 
## use only the first instance of the country 
## in the data and remove the Czech Republic
## and Slovakia from the dataset. 
tmp <- wvs %>% 
  dplyr::select(secpay, gini_disp, democrat, country, wave) %>% 
  filter(wave %in% 2:3) %>% 
  group_by(country) %>% 
  arrange(wave) %>% 
  slice_head(n=1) %>% 
  na.omit() %>% 
  ungroup %>% 
  filter(!(country %in% c("Czech Rep.", "Slovakia"))) %>% 
  mutate(democrat = factor(democrat, labels=c("New Democracy", "Established Democracy")))


## Make the left- and right- basis functions, the .34
## is the knot location. 
BL <- function(x,c=.34)ifelse(x < c, c-x, 0)
BR <- function(x,c=.34)ifelse(x > c, x-c, 0)

## estimate the regression spline model, 
ms <- lm(secpay ~ BL(gini_disp) + BR(gini_disp) + democrat, data=tmp)
## estimate the linear interaction model
mi <- lm(secpay ~ gini_disp*democrat, data=tmp)
## estimate the second degree polynomial model
mp <- lm(secpay ~ gini_disp + I(gini_disp^2) + democrat, data=tmp)

## make a hypothetical dataset where gini_disp varies over its 
## range and democrat takes on both of its possible values
fake <- expand.grid(
  gini_disp = seq(.22, .54 ,by=.01), 
  democrat = factor(c(1,2), labels=c("New Democracy", "Established Democracy"))
)

## generate fitted values from each model using the hypothetical data
fake$fi <- predict(mi, newdata=fake)
fake$fs <- predict(ms, newdata=fake)
fake$fp <- predict(mp, newdata=fake)


## A. Model 8c: Linear Interaction
ggplot() + 
  geom_point(data=tmp, aes(x=gini_disp, y=secpay, shape=democrat, colour=democrat)) + 
  scale_shape_manual(values=c(1, 16)) + 
  geom_line(data=fake, aes(x=gini_disp, y=fi, colour=democrat)) + 
  theme_classic() + 
  theme(legend.position="top") + 
  scale_colour_manual(values=c("black", "gray50")) + 
  labs(x="Gini Coefficient", y="Attitudes Toward Inequality", 
       shape="", colour="")
ggsave("output/f8_6a.png", height=4.5, width=4.5, units="in", dpi=300)

## B. Model 8d: Quadratic Polynomial
ggplot() + 
  geom_point(data=tmp, aes(x=gini_disp, y=secpay, shape=democrat, colour=democrat)) + 
  scale_shape_manual(values=c(1, 16)) + 
  geom_line(data=fake, aes(x=gini_disp, y=fp, colour=democrat)) + 
  theme_classic() + 
  theme(legend.position="top") + 
  scale_colour_manual(values=c("black", "gray50")) + 
  labs(x="Gini Coefficient", y="Attitudes Toward Inequality", 
       shape="", colour="")
ggsave("output/f8_6b.png", height=4.5, width=4.5, units="in", dpi=300)

## C. Model 8e: Piecewise Spline
ggplot() + 
  geom_point(data=tmp, aes(x=gini_disp, y=secpay, shape=democrat, colour=democrat)) + 
  scale_shape_manual(values=c(1, 16)) + 
  geom_line(data=fake, aes(x=gini_disp, y=fs, colour=democrat)) + 
  theme_classic() + 
  theme(legend.position="top") + 
  scale_colour_manual(values=c("black", "gray50")) + 
  labs(x="Gini Coefficient", y="Attitudes Toward Inequality", 
       shape="", colour="")
ggsave("output/f8_6c.png", height=4.5, width=4.5, units="in", dpi=300)
