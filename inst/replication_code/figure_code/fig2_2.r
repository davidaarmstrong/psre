## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(ggthemes)

## load data from psre package
data(inc_ineq)

## filter to just USA between 1990 andn 2018
us <- inc_ineq %>% filter(country == "United States" & 
                            year >= 1990 & year <=2018 )

## The code below allows us to make two axes.  In 
## ggplot2, the second axis has to be plotted in the units
## of the first axis and then the labels can be identified
## throuth the appropriate transformation.  To do this, we
## first need to mape the values of the Gini coefficient
## onto those of the GDP/capita variable.  We do this by 
## finding the minimum and maximum of each and finding the
## slope and intercept of the line that connects them. 
trans.mod <- us %>% summarise(gini = list(range(gini)),
                              gdp_cap = list(range(gdp_cap))) %>%
  unnest(cols=c(gdp_cap, gini)) %>% 
  lm(gdp_cap ~ gini, data=.)
## trans.b is a vector containing the intercept and slope
## of the line connecting (min(gdp),min(gini)) to (max(gdp), max(gini)).  
trans.b <- coef(trans.mod)

## We then make a transformation function that takes values of Gini and
## recasts them as values of GDP/capita
trans <- function(x){trans.b[1] + trans.b[2]*x}

## We also need to make an inverse transformation function.  To do this, 
## we follow the same steps as above, but make gini the dependent variable
## and GDP the independent variable.  
invtrans.mod <- us %>% summarise(gini = list(range(gini)),
                                 gdp_cap = list(range(gdp_cap))) %>%
  unnest(cols=c(gdp_cap, gini)) %>% 
  lm(gini ~ gdp_cap, data=.)
invtrans.b <- coef(invtrans.mod)
invtrans <- function(x){invtrans.b[1] + invtrans.b[2]*x}

## We can calculate the aspect ratio that banks the slopes to 45 degrees
## as per William Cleveland's suggestion 
b45 <- bank_slopes(rep(us$year, 2), c(us$gdp_cap, trans(us$gini)))

## create the base figure
f2_2 <- ggplot(us, aes(x=year)) + 
  geom_line(aes(y = gdp_cap, colour="GDP/capita", linetype="GDP/capita")) + 
  ## note that in the geom_line function below, we use the transformed values of gini
  ## in the plot by calling the trans() function. 
  geom_line(aes(y=trans(gini), colour="Gini Coefficient", linetype="Gini Coefficient")) + 
  theme_classic() + 
  ## note that in the sec_axis statement, which draws a second axis, we identify the 
  ## transformation function as the inverse transformation function defined above. 
  scale_y_continuous(sec.axis = sec_axis(trans = invtrans, name = "Gini Coefficient")) + 
  scale_colour_manual(values=c("black", "gray50")) + 
  scale_linetype_manual(values=c(2,1)) + 
  theme(legend.position=c(.8, .15)) +
  labs(x="Year", y="GDP/capita", colour="", linetype="") 

## Panel (a): Aspect Ratio = 1
f2_2 + 
  theme(aspect.ratio=1) 
ggsave("output/f2_2a.png", height=4.5, width=4.5, dpi=300)
## Panel (b): Aspect Ratio = 1.5
f2_2 + 
  theme(legend.position = c(.8, .2), 
        aspect.ratio=1/1.5) 
ggsave("output/f2_2b.png", height=4.5, width=4.5, dpi=300)

## Panel (c): Banking to 45 degrees
f2_2 + 
  theme(legend.position = c(.8, .2), 
        aspect.ratio=b45, 
        legend.background = element_blank()) 
ggsave("output/f2_2c.png", height=4.5, width=4.5, dpi=300)

