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

## make the average marginal effect plot data
gc1 <- aveEffPlot(moda, "age", gss, nvals = 50, plot=FALSE)

## Get the average case effect plot data
ga1 <- ggpredict(moda, terms="age [n=50]")

## add variables indicating approach to each dataset
ga1 <- ga1 %>% dplyr::select(x, predicted, conf.low, conf.high) %>% 
  mutate(type=factor(1, levels=1:2, labels=c("Average Case", "Observed Case")))
gc1 <- gc1$ci %>% as.data.frame  %>% setNames(c("x", "predicted", "conf.low", "conf.high")) %>% 
  mutate(type=factor(2, levels=1:2, labels=c("Average Case", "Observed Case")))

## put both effect datasets together
ep1 <- bind_rows(ga1, gc1)

## make main panel
g10_5a <- ggplot(ep1, aes(x=x, y=predicted, 
                          ymin=conf.low, ymax=conf.high)) + 
  geom_ribbon(aes(group=type), show.legend=FALSE, alpha=.25) + 
  geom_line(aes(linetype=type)) + 
  theme_classic() + 
  theme(legend.position = c(.2, .9)) + 
  labs(x="Age", y="Expected Number of Children", linetype="")

## make auxiliary histogram for above main panel
g10_5b <- ggplot(gss, aes(x=age)) + 
  geom_histogram(fill="gray75", col="white", bins=15) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(colour="transparent"),
        axis.text.y= element_text(colour="transparent"),
        axis.ticks.y = element_line(colour="transparent")) + 
  coord_cartesian(xlim=c(18,89), expand=FALSE)

## put two figures together
#png("output/f10_4.png", height=5.5, width=4.5, units="in", res=300)  
grid.arrange(g10_5b, g10_5a, ncol=1, heights=c(2,8))
#dev.off()
