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

## get the x- and y-axis limits for the a and b panels. 
rg1 <- ggplot_build(dfbhist(dbd, "dfb_ginil", "country"))$layout$panel_params[[1]][c("x.range", "y.range")]
rg2 <- ggplot_build(dfbhist(dbd, "dfb_democl", "country", binwidth=.015))$layout$panel_params[[1]][c("x.range", "y.range")]

## A. Model 7a: Gini Coefficient
dfbhist(dbd, "dfb_ginil", "country", cutval=.2)
ggsave("output/f7_8a.png", height=4.5, width=4.5, units="in", dpi=300)

## B. Model 7a: Democratic History
dfbhist(dbd, "dfb_democl", "country", cutval = .2, binwidth=.015)
ggsave("output/f7_8b.png", height=4.5, width=4.5, units="in", dpi=300)

## C. Model 7c: Gini Coefficient
dfbhist(dbd, "dfb_ginip", "country", xrange = rg1$x.range, 
        nudge_y=c(0,2,0))
ggsave("output/f7_8c.png", height=4.5, width=4.5, units="in", dpi=300)

## D. Model 7c: Democratic History
dfbhist(dbd, "dfb_democp", "country", binwidth=.01, xrange = rg2$x.range)
ggsave("output/f7_8d.png", height=4.5, width=4.5, units="in", dpi=300)
