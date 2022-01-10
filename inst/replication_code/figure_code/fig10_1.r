## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(rio)
library(ggrepel)

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

## use the first observation from the gss data 
## to make predictions.  we are going to hold 
## all other variables constant at the values 
## of the first observation.  Then we will change 
## the values of age by five units - 2.5 less 
## than the age for the first observation and 
## 2.5 more than age for the first observation. 
fake <- gss[c(1,1),] 
## change age by 5 years for the two observations
fake$age <- fake$age + c(-2.5, 2.5)
## generate predictions for the two observations 
fit <- predict(moda, newdata= fake, type="response")

## make the data that will be used in the plot. 
plot_pois <- tibble(
  x = rep(fit, each=9), 
  person = factor(rep(c("M (mu = 1.13)", "F (mu = 1.85)"), each=9), 
                  levels=c("M (mu = 1.13)", "F (mu = 1.85)")), 
  vals = rep(0:8, 2), 
  ## here we use the poisson pmf to get the 
  ## predicted count probabilities.
  pois = dpois(0:8, x)
)

## A. Poisson Probabilities
ggplot(plot_pois, aes(x=vals, y=pois , fill=person)) + 
  geom_bar(position="dodge", stat="identity") + 
  theme_classic() + 
  scale_fill_manual(values=c("black", "gray50")) + 
  labs(x="Number of Childern", y="Poisson Probability", 
       fill="Respondent")+ 
  theme(legend.position = c(.85, .85)) 
ggsave("output/f10_1a.png", height=4.5, width=4.5, units="in", dpi=300)


## get coefficients from the model
b <- coef(moda)

## values of age used to make the lines
age <- 18:89
## linear predictor for all values of age 
## between 18 and 89 holding everything else
## constant at central values. 
eta_am <- b[1] + b[2]*age + b[3]* .26 + b[4]*0 + b[5]*12
eta_af <- b[1] + b[2]*age + b[3]* .26 + b[4]*1 + b[5]*12
## linear predictor for hypothetical cases 
## at 20 and 45
eta_m <- b[1] + b[2]*20 + b[3]* .26 + b[4]*0 + b[5]*12
eta_f <- b[1] + b[2]*45 + b[3]* .26 + b[4]*1 + b[5]*12

## marginal effect for the poisson model 
## is exp(eta)*b
## slope of line tangent to the curve for men
slope_m <- exp(eta_m)*coef(moda)[2]
## intercept of the line tangent to the curve
## for men 
int_m <- exp(eta_m)-slope_m*20
## slope of the line tangent to the curve for 
## women
slope_f <- exp(eta_f)*coef(moda)[2]
## intercept of the line tangent to the curve
## for women
int_f <- exp(eta_f)-slope_f*45

## make hypothetical data for men that has 
## all ages between 18 and 89 and the 
## predicted mu
plot_m <- tibble(
  age = age, 
  eta = eta_am, 
  sex = factor(1, levels=1:2, labels=c("M", "F")))
## make hypothetical data for women that has 
## all ages between 18 and 89 and the 
## predicted mu
plot_f <- tibble(
  age = age, 
  eta = eta_af, 
  sex = factor(2, levels=1:2, labels=c("M", "F")))
## put the data for men and women together. 
plot_b <- bind_rows(
  plot_m, plot_f)

## make a character that will serve as the label 
## {M, F} where the age is at its maximum
plot_b <- plot_b %>% 
  mutate(lab = case_when(age == max(age) ~ as.character(sex), TRUE~NA_character_))

## B. Poisson Marginal Effect
ggplot(plot_b, aes(x=age, y=exp(eta), group=sex)) + 
  geom_line() + 
  geom_text_repel(aes(label = lab), hjust=1, vjust=.5, position=position_nudge(x=c(0,0))) + 
  geom_point(x=20, y=exp(eta_m)) + 
  geom_point(x=45, y=exp(eta_f)) + 
  geom_abline(slope=slope_m, intercept = int_m, linetype=2) + 
  geom_abline(slope=slope_f, intercept = int_f, linetype=3) + 
  theme_classic() + 
  labs(x="Age", y="E(y|b,X)")
ggsave("output/f10_1b.png", height=4.5, width = 4.5, units="in", dpi=300)
