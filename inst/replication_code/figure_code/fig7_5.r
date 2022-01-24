## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(ggrepel)

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
pmod3 <- lm(secpay ~ poly(gini_disp, 3) + democrat, data=wvs0)
## linear additive model 
lmod <- lm(secpay ~ gini_disp + democrat, data=wvs0)

## keep only necessary variables
w0 <- wvs0 %>% dplyr::select(democrat, gini_disp, secpay, country, year, wave)

## get residuals, predictions, etc... from the linear model 
m0data <- broom::augment(lmod)
## join m0data with original data
m0data <- left_join(m0data, w0)
## add in studentized residuals
m0data$.rstud <- rstudent(lmod)
## get model matrix from linear model 
m0X <- model.matrix(lmod)
## calculate the number of parameters divided by 
## the number of observations
pon <- ncol(m0X)/nrow(m0X)
## calculate cut points for leverage and residuals
.h_cut <- 2*pon
.e_cut <- 2
## identify points that are above the cutpoint 
## either on residuals or leverage
m0data <- m0data %>% 
  mutate(text = case_when(
    .hat > .h_cut | abs(.rstud) > .e_cut ~ country, 
    TRUE ~ NA_character_))

ggplot(m0data, aes(x=.hat, y=.rstud)) + 
  geom_point(aes(size=.cooksd), shape=1, show.legend=FALSE) + 
  geom_vline(xintercept=c(2,3)*pon, lty=2) + 
  geom_hline(yintercept=c(-2, 2, 0), lty=2) + 
  geom_text_repel(aes(label=text), hjust=1, nudge_x=c(.02, .04, .0305)) + 
  theme_classic() + 
  labs(x="Hat Values", y="Studentized Residuals") 
# ggssave("output/f7_5a.png", height=4.5, width=4.5, units="in", dpi=300)

## proceed as above, but using the data from the cubic polynomial
m0data <- broom::augment(pmod3)
m0data <- left_join(m0data, w0)
m0data$.rstud <- rstudent(pmod3)
m0X <- model.matrix(pmod3)
pon <- ncol(m0X)/nrow(m0X)
.h_cut <- 2*pon
.e_cut <- 2
m0data <- m0data %>% 
  mutate(text = case_when(
    .hat > .h_cut | abs(.rstud) > .e_cut ~ country, 
    TRUE ~ NA_character_))

ggplot(m0data, aes(x=.hat, y=.rstud)) + 
  geom_point(aes(size=.cooksd), shape=1, show.legend=FALSE) + 
  geom_vline(xintercept=c(2,3)*pon, lty=2) + 
  geom_hline(yintercept=c(-2, 2, 0), lty=2) + 
  geom_text_repel(aes(label=text), hjust=1, nudge_x=c(0.087,0.085, 0.04, 0.055, 0)) + 
  theme_classic() + 
  labs(x="Hat Values", y="Studentized Residuals") 
# ggssave("output/f7_5b.png", height=4.5, width=4.5, units="in", dpi=300)

