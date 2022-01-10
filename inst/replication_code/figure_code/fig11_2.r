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

## calculate average first difference of each variable
## in the model for a 2 sd change 
g_eth <- glmChange2(mod1, "eth", india, diffchange="sd", n=2)
g_sbc <- glmChange2(mod1, "sbc", india, diffchange="sd", n=2)
g_ed <- glmChange2(mod1, "educyrs", india, diffchange="sd", n=2)
g_tb <- glmChange2(mod1, "tb3", india)
g_urban <- glmChange2(mod1, "urban", india, diffchage="sd", n=2)
g_ai <- glmChange2(mod1, "anti_immigration", india, diffchage="sd", n=2)

## combine results in a list
res_list <- list(
  g_eth, g_sbc, g_ed, g_tb, g_urban, g_ai)

## make res - a matrix of results from 
## the average first differences
res <- rbind(
  g_eth$res, 
  g_sbc$res, 
  g_ed$res, 
  g_tb$res,
  g_urban$res, 
  g_ai$res
)

## Turn res into a data frame
res <- as.data.frame(res)
## sort res by effect size
res <- res %>% arrange(mean)
## make var a factor that identifies
## the variable and optionally the contrast
## for factors. 
res <- res %>% mutate(var = factor(1:6, 
                                   labels=c("Ethnicity\n(Muslim-Hindu)", 
                                            "Income Group\n(Top-Middle)", 
                                            "Scheduled or Backward\nCaste", 
                                            "Anti-immigrant\nAttitudes", 
                                            "Years of Formal\nEducation", 
                                            "Urban Resident")))

## make plot
ggplot(res, aes(x=mean, xmin=lower, xmax=upper, y=var)) + 
  geom_pointrange() + 
  theme_classic() + 
  geom_vline(xintercept=0, lty=3) + 
  labs(x="Change in Predicted Probabilities\n(95% Confidence Interval)", y="")
ggsave("output/f11_2.png", height=4.5, width=4.5, units="in", dpi=300)




