## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(DAMisc)

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

## get all the variables that were in 
## mod1 from the india dataset and 
## listwise delete
d <- get_all_vars(mod1, india) %>% na.omit()

## calculate the education average 
## first difference. 
g_ed <- glmChange2(mod1, "educyrs", india, diffchange="sd", n=2)

## make a data frame of the education effects
ed_eff <- tibble(
  eth = d$eth, 
  eff = g_ed$avesamp
)

## A. Distribution of first differences of
## education by ethnicity
ggplot(ed_eff, aes(x=eff, fill=eth)) +
  geom_histogram(position="identity", alpha=.5) +
  theme_classic() +
  theme(legend.position=c(.15,.9)) +
  scale_fill_manual(values=c("gray75", "gray50", "gray25")) +
  labs(x="First Difference of Education", y="Frequency", fill="")
ggsave("output/f11_3a.png", height=4.5, width=4.5, units="in", dpi=300)

## calculate the linear predictor for 
## mod1 and add in ethnicity
eta_dat <- data.frame(
  eta = predict(mod1, type="link"), 
  eth = d$eth
)

## find the ranges of and also 
## the 2.5th and 97.5th percentile
## of eta for the different ethnicities
eta_sum <- eta_dat %>% 
  group_by(eth) %>% 
  summarise(min = min(eta), 
            max = max(eta), 
            low = quantile(eta, .025), 
            up = quantile(eta, .975))

## B. Distribution of eta by ethnicity
ggplot() + 
  geom_line(aes(x=seq(-4, 1.5, length=100), y=plogis(seq(-4, 1.5, length=100))), col="black") + 
  theme_classic() + 
  theme(legend.position=c(.15,.9)) +
  geom_segment(aes(x=eta_sum$min[2], y=0, xend=eta_sum$max[2], yend=0), 
               size=2) + 
  geom_segment(aes(x=eta_sum$min[1], y=0, xend=eta_sum$max[1], yend=0), 
               size=2) + 
  scale_colour_manual(values=c("black", "gray50")) +
  geom_text(aes(x=mean(c(eta_sum$min[2], eta_sum$max[2])), 
                y=-0.03, label="Muslim")) + 
  geom_text(aes(x=mean(c(eta_sum$min[1], eta_sum$max[1])), 
                y=-0.03, label="Hindu")) + 
  labs(x=expression(eta), y="E(y|b,x1)", colour="", parse=TRUE)
ggsave("output/f11_3b.png", height=4.5, width=4.5, units="in", dpi=300)


