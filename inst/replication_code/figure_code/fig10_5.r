## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(rio)
library(DAMisc)
library(ggeffects)
library(gridExtra)

## load data from psre package
data(gss)

## decrease the variance of sei01, make sex a factor, 
## and recode values of education less than 6 to 6. 
## select only the required variables, then listwise
## delete. 
gss <- gss %>% 
  mutate(sei01 = sei10/100, 
         sex = factorize(sex),
         educ = case_when(
           educ < 6 ~ 6, 
           TRUE ~ educ)) %>% 
  dplyr::select(childs, age, sei01, sex, educ) %>% 
  na.omit()

## estimate a poisson glm
moda <- glm(childs ~ sei01 + sex + educ + age, 
            data=gss, family=poisson)

## Calculate Silber et. al. importance 
## measures for model 
s_imp <- srr_imp(moda, data=gss)

## calculate Greenwell importance 
## measures for each variable from 
## the model. 
g_imp1 <- glmImp(moda, "age", gss)
g_imp2 <- glmImp(moda, "sei01", gss)
g_imp3 <- glmImp(moda, "sex", gss)
g_imp4 <- glmImp(moda, "educ", gss)

## put the greenwell importance measure
## results together and add rownames
## to the dataset
g_imp <- bind_rows(g_imp1, g_imp2, g_imp3, g_imp4)
rownames(g_imp) <- c("age", "sei01", "sex", "educ")

## set the names of the greenwell dataset and 
## add a variable indicating the method used. 
g_imp <- as_tibble(g_imp, rownames="var") %>% 
  setNames(c("var", "importance", "lwr", "upr")) %>% 
  mutate(method=factor(2, levels=1:2, labels=c("Silber", "Greenwell")))

## add a variable indicating the method used. 
s_imp <- as_tibble(s_imp) %>% 
  mutate(method=factor(1, levels=1:2, labels=c("Silber", "Greenwell")))

## put the two results together 
imp <- bind_rows(s_imp, g_imp)

## make more appropriate variable labels
imp <- imp %>% 
  mutate(var = factor(var, levels=c("educ", "age", "sex", "sei01"), 
                      labels=c("Education", "Age", "Sex", "SES")))

## make plot
ggplot(imp, aes(y=reorder(var, importance, mean), x=importance, xmin=lwr, xmax=upr, group=method)) + 
  geom_errorbarh(aes(linetype=method), position = position_dodge(width=.5), height=0) + 
  geom_point(aes(shape=method), position=position_dodge(width=.5)) +
  scale_shape_manual(values=c(1,16)) + 
  theme_classic() + 
  theme(legend.position = c(.8, .2)) + 
  labs(y="", x="Variable Importance", linetype="", shape="") 
# ggssave("output/f10_5.png", width=4.5, height=4.5, units="in", dpi=300)
