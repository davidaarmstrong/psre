## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages

library(tidyverse)
library(psre)
library(car)
library(cellWise)
#library(sn)

## load data from psre package
data(wvs)
## rename rgdpe -> gdp
wvs <- wvs %>% mutate(gdp = rgdpe)

## get BCn transform of gdp and population
lgdp <- powerTransform(wvs$gdp, family="bcnPower")
lpop <- powerTransform(wvs$pop, family="bcnPower")
trans_gdp <- bcnPower(wvs$gdp, lambda=lgdp$lambda, gamma=lgdp$gamma)
trans_pop <- bcnPower(wvs$pop, lambda=lpop$lambda, gamma=lpop$gamma)

## calculate lambda for Raymaekers transformation for gdp and pop
rrtrans <- transfo(wvs[,c("gdp", "pop")], type="bestObj")

## calculate lambda for Velez transformation for both variables
vtrans_gdp <- transNorm(wvs$gdp, family = "bc", lams=c(0.00001, 1))
vtrans_pop <- transNorm(wvs$pop, family = "bc", lams=c(0, 1))

## implment Raymaekers and Velez transformations
trans_gdp1 <- bcPower(wvs$gdp, lambda=rrtrans$lambdahats[1])
trans_pop1 <- bcPower(wvs$pop, lambda=rrtrans$lambdahats[2])
trans_gdp2 <- bcPower(wvs$gdp, lambda=vtrans_gdp)
trans_pop2 <- bcPower(wvs$pop, lambda=vtrans_pop)

## calculate K-S tests for GDP and transformations
gdp_ks0 <- fBasics::normalTest(wvs$gdp, method=c("ks"))
gdp_ks <- fBasics::normalTest(trans_gdp, method=c("ks"))
gdp_ks1 <- fBasics::normalTest(trans_gdp1, method=c("ks"))
gdp_ks2 <- fBasics::normalTest(trans_gdp2, method=c("ks"))

## calculate K-S test for population and transformations
pop_ks0 <- fBasics::normalTest(wvs$pop, method=c("ks"))
pop_ks <- fBasics::normalTest(trans_pop, method=c("ks"))
pop_ks1 <- fBasics::normalTest(trans_pop1, method=c("ks"))
pop_ks2 <- fBasics::normalTest(trans_pop2, method=c("ks"))

## calculate S-W test for GDP and transformations
gdp_sw0 <- fBasics::normalTest(wvs$gdp, method=c("sw"))
gdp_sw <- fBasics::normalTest(trans_gdp, method=c("sw"))
gdp_sw1 <- fBasics::normalTest(trans_gdp1, method=c("sw"))
gdp_sw2 <- fBasics::normalTest(trans_gdp2, method=c("sw"))

## calculate S-W test for Population and transformations
pop_sw0 <- fBasics::normalTest(wvs$pop, method=c("sw"))
pop_sw <- fBasics::normalTest(trans_pop, method=c("sw"))
pop_sw1 <- fBasics::normalTest(trans_pop1, method=c("sw"))
pop_sw2 <- fBasics::normalTest(trans_pop2, method=c("sw"))

## calculate D-P test for GDP and transformations
gdp_dago0 <- fBasics::dagoTest(wvs$gdp)
gdp_dago <- fBasics::dagoTest(trans_gdp)
gdp_dago1 <- fBasics::dagoTest(trans_gdp1)
gdp_dago2 <- fBasics::dagoTest(trans_gdp2)

## caluclate D-P test for Population and Transformations
pop_dago0 <- fBasics::dagoTest(wvs$pop)
pop_dago <- fBasics::dagoTest(trans_pop)
pop_dago1 <- fBasics::dagoTest(trans_pop1)
pop_dago2 <- fBasics::dagoTest(trans_pop2)


## collate results for GDP
ks <- c(gdp_ks0@test$statistic, gdp_ks@test$statistic, gdp_ks1@test$statistic, gdp_ks2@test$statistic, 
        pop_ks0@test$statistic, pop_ks@test$statistic, pop_ks1@test$statistic, pop_ks2@test$statistic)
sw <- c(gdp_sw0@test$statistic[1], gdp_sw@test$statistic[1], gdp_sw1@test$statistic[1], gdp_sw2@test$statistic[1], 
        pop_sw0@test$statistic[1], pop_sw@test$statistic[1], pop_sw1@test$statistic[1], pop_sw2@test$statistic[1])
dag <- c(gdp_dago0@test$statistic[1], gdp_dago@test$statistic[1], gdp_dago1@test$statistic[1], gdp_dago2@test$statistic[1], 
        pop_dago0@test$statistic[1], pop_dago@test$statistic[1], pop_dago1@test$statistic[1], pop_dago2@test$statistic[1])

## collate results for population
ksp <- c(gdp_ks0@test$p.value[1], gdp_ks@test$p.value[1], gdp_ks1@test$p.value[1], gdp_ks2@test$p.value[1], 
        pop_ks0@test$p.value[1], pop_ks@test$p.value[1], pop_ks1@test$p.value[1], pop_ks2@test$p.value[1])
swp <- c(gdp_sw0@test$p.value[1], gdp_sw@test$p.value[1], gdp_sw1@test$p.value[1], gdp_sw2@test$p.value[1], 
        pop_sw0@test$p.value[1], pop_sw@test$p.value[1], pop_sw1@test$p.value[1], pop_sw2@test$p.value[1])
dagp <- c(gdp_dago0@test$p.value[1], gdp_dago@test$p.value[1], gdp_dago1@test$p.value[1], gdp_dago2@test$p.value[1], 
         pop_dago0@test$p.value[1], pop_dago@test$p.value[1], pop_dago1@test$p.value[1], pop_dago2@test$p.value[1])

## identify significant results
ksv <- ifelse(ksp < .05, sprintf("%.2f*", ks), sprintf("%.2f", ks))
swv <- ifelse(swp < .05, sprintf("%.2f*", sw), sprintf("%.2f", sw))
dagv <- ifelse(dagp < .05, sprintf("%.2f*", dag), sprintf("%.2f", dag))

## make table
tab_3.2 <- tibble(
  Variable = rep(c("GDP", "Population"), each=4), 
  Test = rep(c("Raw", "BCn", "Velez", "Raymaekers"), 2), 
  KS = ksv, 
  SW = swv, 
  DP = dagv
)

tab_3.2



