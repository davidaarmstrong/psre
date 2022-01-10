## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(clarkeTest)
library(rsample)
library(lmtest)
library(stargazer)

## load data from psre package
data(wvs)

## select relevant variables, listwise delete and 
## generate pct_su_plus variable
tmp <- wvs %>% dplyr::select(pct_secondary, pct_some_univ, pct_univ_degree, 
                             pct_low_income, pct_high_income, pct_female, 
                             mean_lr, resemaval, sacsecval, moral, democrat) %>% 
  na.omit() %>% 
  mutate(pct_su_plus = pct_some_univ + pct_univ_degree)

## estimate four differently specified models
m1 <- lm(resemaval ~ pct_su_plus + democrat, data=tmp)
m2 <- lm(resemaval ~ pct_high_income + democrat, data=tmp)
m3 <- lm(resemaval ~ pct_female + democrat, data=tmp)
m4 <- lm(resemaval ~ pct_su_plus + pct_high_income + pct_female + democrat, data=tmp)

## calculate the AIC and BIC deltas for the models 
delta_AIC <- AIC(m1, m2, m3, m4) - min(AIC(m1, m2, m3, m4))
delta_BIC <- BIC(m1, m2, m3, m4) - min(BIC(m1, m2, m3, m4))

## calculate the LR test for each model against model 4 (the full model)
l1 <- lrtest(m1, m4)
l2 <- lrtest(m2, m4)
l3 <- lrtest(m3, m4)

## write function to calculate the cross-validation error
## for each model. 
cv_mods <- function(split, ...){
  tm1 <- update(m1, data=analysis(split))
  tm2 <- update(m2, data=analysis(split))
  tm3 <- update(m3, data=analysis(split))
  tm4 <- update(m4, data=analysis(split))
  f1 <- predict(tm1, newdata=assessment(split))
  f2 <- predict(tm2, newdata=assessment(split))
  f3 <- predict(tm3, newdata=assessment(split))
  f4 <- predict(tm4, newdata=assessment(split))
  y <- assessment(split)$resemaval
  e1 <- y-f1
  e2 <- y-f2
  e3 <- y-f3
  e4 <- y-f4
  data.frame(e1 = sum(e1^2), e2 = sum(e2^2), e3 = sum(e3^2), e4 = sum(e4^2))
}

## caluclate cross-validation error for each model
out <- vfold_cv(tmp,
                v=10,
                repeats = 500) %>%
  mutate(err = map(splits,
                   cv_mods)) 

## unnest CV results into a table 
out2 <- out %>% 
  unnest(err)  %>% 
  group_by(id) %>% 
  summarise(across(starts_with("e"), ~sum(.x)))
## aggregate the cross-validation error
cve <- out2 %>% 
  summarise(across(starts_with("e"), ~mean(.x))) %>% 
  as.matrix() %>% 
  c(.)

## make bottom panel of table
other_stats <- matrix(nrow=4, ncol=4)
other_stats[1, ] <- delta_AIC[,2]
other_stats[2,] <- delta_BIC[,2]
other_stats[3,] <- cve
other_stats[4,] <- c(l1[2,4], l2[2,4], l3[2,4], NA)
colnames(other_stats) <- c("Model 5d", "Model 5e", "Model 5f", "Model 5g")
rownames(other_stats) <- c("Delta AIC", "Delta BIC", "CV Error", "LR Test vs 5g")

## print tables - note, the single table 5.2 in the 
## text is printed as two separate tables here.
stargazer(m1, m2, m3, m4, digits=3, type="text")
round(other_stats, 2)
