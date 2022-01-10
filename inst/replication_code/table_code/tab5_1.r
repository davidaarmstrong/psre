## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(stargazer)

## load data from psre package
data(ces)

## add row number (observation) variable to ces
ces$rownum <- 1:nrow(ces)

## make gender a factor increase variance in moral and 
## decrease variance in leader_con 
ces <- ces %>% 
  mutate(gender = factor(gender, levels=c(1,5), labels=c("male", "female")), 
         moral = (moral + 1)*50, 
         leader_con = leader_con/20)

## identify row numbers to be extracted from data
rn <- c(52, 133, 175, 193, 285, 320, 336, 428, 543, 560, 650, 695, 709, 804, 913,
        974,1065,1172,1186,1220,1235,1237,1307,1365,1420,1484,1517,1523,1554,1609,
        1684,1690,1768,1918,1970,1986,2053,2067,2137,2229,2239,2371,2403,2432,2515,
        2591,2594,2701,2759,2799)

## extract only identified rows from data
ces_samp <- ces %>% 
  filter(rownum %in% rn)

## estimate models
m1 <- lm(moral ~ leader_con, data=ces_samp)
m2 <- lm(moral ~ gender, data=ces_samp)
mod <- lm(moral ~ gender + leader_con, data=ces_samp)

stargazer(m1, m2, mod, digits=2, type="text")
