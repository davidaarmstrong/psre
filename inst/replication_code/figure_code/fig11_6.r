## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(separationplot)
library(mgcv)
## load data from psre package
data(india)

## Manage india election data
india <- india %>% 
  ## make urban a binary variable
  mutate(urban = case_when(
    urbrural %in% c(1,2) ~ 1, 
    urbrural %in% 3:5 ~ 0, 
    TRUE ~ NA_real_), 
    ## make urban and sbc factors
    urban = as.factor(urban), 
    sbc = as.factor(sbc), 
    ## make bjp a dummy variable indicating
    ## bjp vote
    bjp = case_when(
      in_prty == 2 ~ 1, 
      in_prty %in% c(1,3,4,5) ~ 0, 
      TRUE ~ NA_real_), 
    ## recode ethnicity into broader categories
    eth = case_when(
      in_ethn1 %in% 1:4 ~ "Hindu", 
      in_ethn1 %in% 5:7 ~ "Muslim", 
      in_ethn1 %in% 8:12 ~ "Other", 
      TRUE ~ NA_character_), 
    eth = as.factor(eth), 
    ## make topbot into a three-category variable
    tb3 = case_when(
      topbot %in% 1:3 ~ "Low", 
      topbot %in% 4:7 ~  "Middle", 
      topbot %in% 8:10 ~ "High", 
      TRUE ~ NA_character_), 
    tb3 = factor(tb3, levels=c("Low", "Middle", "High")))

## estimate logit model of bjp vote
mod1 <- glm(bjp ~  eth + sbc + educyrs + tb3 + urban + 
              anti_immigration, 
            data=india, family=binomial)

## get predicted data both on the response (probability) 
## and link (log-odds) scales along with the original 
## y variable and sort by the predicted probability
eta_dat <- data.frame(
  eta = predict(mod1, type="link"), 
  prob = predict(mod1, type="response"), 
  yobs = model.response(model.frame(mod1))) %>% 
  arrange(prob) %>% 
  mutate(obs = 1:n())

## generate a prediction of a smoothing spline (GAM) 
## of the predicted probability on the observation 
## number
fit_dat <- eta_dat %>% 
  mutate(fit = predict(gam(prob ~ s(obs), data=.)), 
         ## constrain the predictions to be 
         ## in the interval (0,1)
         fit = ifelse(fit < 0, 0, fit))

## make data that will allow us to make polygons
## for each observation.  The polygon will be drawn
## from (x1,y1) -> (x2,y2) -> (x3,y3) -> (x4,y4) -> (x5,y5)
eta_dat <- eta_dat %>% 
  mutate(x1 = obs - .5,
         x2 = obs + .5, 
         x3 = obs + .5, 
         x4 = obs - .5, 
         x5 = obs - .5, 
         y1 = 0, 
         y2 = 0, 
         y3 = 1, 
         y4 = 1, 
         y5 = 1)

## pivot the data to longer
eta_dat <- eta_dat %>% pivot_longer(x1:y5, names_pattern="([xy])(\\d)", names_to=c(".value", "num"))

## make plot
ggplot() + 
  geom_polygon(data=eta_dat, aes(x=x, y=y, group=obs, fill=as.factor(yobs)), color="transparent", show.legend=FALSE) + 
  geom_line(data=fit_dat, aes(x=obs, y=fit), col="black") + 
  scale_fill_manual(values=c("gray35", "gray65")) + 
  theme_void()
ggsave("output/f11_5.png", height=.5, width=6, units="in", dpi=300)


## A much simpler, though less flexible way of doing this is with 
## separationplot()
separationplot(fitted(mod1), model.response(model.frame(mod1)), 
                col0 = "gray30", col1="gray60", file=NULL, shuffle=FALSE)

