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
  slice_head(n=1) 

## Fit different models for the two IVs 
## third-degree polynomial in gini
pmod <- lm(secpay ~ poly(gini_disp, 3) + democrat, data=wvs0)
## linear additive model 
lmod <- lm(secpay ~ gini_disp + democrat, data=wvs0)

## calculate dfbetas for both models
dba <- dfbetas(lmod)
dbc <- dfbetas(pmod)

## make model frame for linear model
mf <- model.frame(lmod)

## join the model frame with the original data
dbd <- left_join(mf, dplyr::select(wvs0, country, secpay, gini_disp))

## for variables that have more than one regressor, like
## the cubic polynomial - sum the squared dfbetas values
## for all coefficients relating to that value. 
dbc2 <- cbind(rowSums(dbc[,2:4]^2), dbc[,5]^2)

## add the dfbetas data into the raw data
dbd$dfb_ginil <- dba[,2]^2
dbd$dfb_democl <- dba[,3]^2
dbd$dfb_ginip <- dbc2[,1]
dbd$dfb_democp <- dbc2[,2]^2


## A. Model 7a: Gini Coefficient
ggplot(dbd, aes(y=reorder(country, dfb_ginil, mean), x=dfb_ginil)) + 
  geom_point() + 
  geom_segment(aes(xend=0, y=country, yend=country)) + 
  theme_classic() + 
  labs(x="Gini Coefficient DFBETAS", y="")
# ggssave("output/f7_7a.png", width=4.5, height=6, units="in", dpi=300)

## B. Model 7a: Democratic History
ggplot(dbd, aes(y=reorder(country, dfb_democl, mean), x=dfb_democl)) + 
  geom_point() + 
  geom_segment(aes(xend=0, y=country, yend=country)) + 
  theme_classic() + 
  labs(x="Democratic History DFBETAS", y="")
# ggssave("output/f7_7b.png", width=4.5, height=6, units="in", dpi=300)

## get the x- and y-axis limits from the plots for model 7a
xrga <- ggplot_build(f7_7a)$layout$panel_params[[1]]$x.range
yrga <- ggplot_build(f7_7a)$layout$panel_params[[1]]$y.range
xrgb <- ggplot_build(f7_7b)$layout$panel_params[[1]]$x.range
yrgb <- ggplot_build(f7_7b)$layout$panel_params[[1]]$y.range

## C. Model 7c: Gini coefficient
ggplot(dbd, aes(y=reorder(country, dfb_ginip, mean), x=dfb_ginip)) + 
  geom_point() + 
  geom_segment(aes(xend=0, y=country, yend=country)) + 
  theme_classic() + 
  labs(x="Gini Coefficient DFBETAS", y="") + 
  coord_cartesian(xlim = xrga, ylim=yrga, expand=FALSE)
# ggssave("output/f7_7c.png", width=4.5, height=6, units="in", dpi=300)

## D. Model 7c: Democratic History
ggplot(dbd, aes(y=reorder(country, dfb_democp, mean), x=dfb_democp)) + 
  geom_point() + 
  geom_segment(aes(xend=0, y=country, yend=country)) + 
  theme_classic() + 
  labs(x="Democratic History DFBETAS", y="") + 
  coord_cartesian(xlim = xrgb, ylim=yrgb, expand=FALSE)
# ggssave("output/f7_7d.png", width=4.5, height=6, units="in", dpi=300)

