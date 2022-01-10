## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(DAMisc)
library(rio)

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
## make the labels for each type of effect
levs <- c("Average Case (2sd)", 
          "Observed Case (2sd)", 
          "Observed Case (2sd, trim)")

## make the average case effect
ga2 <- glmChange(moda, gss, diffchange="sd", n=2, sim=TRUE)
## select the required variables and 
## change the names. 
ga2 <- ga2 %>% dplyr::select(focal, diff, `q_2.5`, `q_97.5`) %>% 
  setNames(c("var", "fit", "low", "up")) %>% 
  mutate(model = factor(1, levels=1:3, labels=levs), 
         fit = as.numeric(fit))

## make the observed case approach 
go2 <- sapply(c("age", "sei01", "sex", "educ"), function(x)
  glmChange2(moda, gss, varname=x, diffchange="sd", n=2, sim=TRUE, trim=FALSE)$res)

## turn the observed case result
## into a data frame with the appropriate
##  variable names
go2 <- t(go2)
colnames(go2) <- c("fit", "low", "up")
go2 <- as_tibble(go2, rownames="var")%>% 
  mutate(model = factor(2, levels=1:3, labels=levs))

## make the trimmed observed case approach
go2t <- sapply(c("age", "sei01", "sex", "educ"), function(x)
  glmChange2(moda, gss, varname=x, diffchange="sd", n=2, sim=TRUE, trim=TRUE)$res)

## turn the trimmed observed case result
## into a data frame with the appropriate
##  variable names
go2t <- t(go2t)
colnames(go2t) <- c("fit", "low", "up")
go2t <- as_tibble(go2t, rownames="var") %>% 
  mutate(model = factor(3, levels=1:3, labels=levs))

## put all effect datasets together
allg <- bind_rows(ga2, go2, go2t) %>% 
  mutate(var = factor(as.character(var), levels=c("age", "sex", "sei01", "educ")))
levels(allg$var) <- c("Age", "Sex", "SES", "Education")

## make plot
ggplot(allg, aes(x=reorder(model, abs(fit), mean), y=fit, ymin=low, ymax=up)) + 
  geom_pointrange(position="dodge") + 
  geom_hline(yintercept=0, lty=2) + 
  facet_grid(var~.) + 
  theme_bw() + 
  theme(panel.grid=element_blank()) + 
  labs(x="", y="First Difference\n(95% Confidence Interval)") + 
  coord_flip()
ggsave("output/f10_3.png", width=5, height=7, units="in", dpi=300)
