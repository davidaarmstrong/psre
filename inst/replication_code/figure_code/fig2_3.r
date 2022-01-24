## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(ggthemes)
library(scales)

## load data from psre package
data(inc_ineq)

## filter to include just 2018 and 
## only contain country and gdp_cap variables
## sort alphabetically by country
ctry_gdp <- inc_ineq %>% 
  filter(year == 2018) %>% 
  select(country, gdp_cap) %>% 
  arrange(country) 

## create cntry5 data frame that includes only the listed countries. 
ctry5 <- ctry_gdp %>% filter(country %in% c("Grenada", "Lebanon", "Denmark", "Qatar", "Morocco"))

## Panel (a): Dot plot
ctry5 %>% arrange(gdp_cap) %>% 
  mutate(ctry_fac = factor(1:n(), labels=country)) %>% 
  ggplot(aes(x=gdp_cap, y=ctry_fac)) + 
  geom_point() + 
  geom_segment(aes(xend=0, yend=ctry_fac)) + 
  theme_classic() + 
  labs(x="GDP/capita", y="") + 
  scale_x_continuous(labels=dollar) + 
  coord_cartesian(xlim=c(0,115000), ylim=c(.5, 5.5), expand=FALSE) 
# ggssave("output/f2_3a.png", height=4.5, width=4.5, dpi=300)
## Panel (b): Bar

ctry5 %>% arrange(gdp_cap) %>% 
  mutate(ctry_fac = factor(1:n(), labels=country)) %>% 
  ggplot(aes(y=gdp_cap, x=ctry_fac)) + 
  geom_bar(stat="identity", fill="gray65") + 
  theme_classic() + 
  labs(y="GDP/capita", x="") + 
  scale_y_continuous(labels=dollar) + 
  coord_cartesian(ylim=c(0,115000), xlim=c(.5, 5.5), expand=FALSE) 
# ggssave("output/f2_3b.png", height=4.5, width=4.5, dpi=300)

## Panel (c): Area
ctry5 %>% arrange(gdp_cap) %>% 
  mutate(ctry_fac = factor(1:n(), labels=country)) %>% 
  ggplot(aes(x=1, y=ctry_fac)) + 
  geom_point(aes(size=gdp_cap)) + 
  scale_size_continuous(range=c(1,20)) + 
  theme_classic() + 
  labs(y="GDP/capita", x="", size="GDP/Capita") 
# ggssave("output/f2_3c.png", height=4.5, width=4.5, dpi=300)

## create a vector of numbers for gdp_cap that is rescaled
## to the range (0,75)
cvec <- -ctry5$gdp_cap
cvec <- cvec-min(cvec)
cvec <- (cvec/max(cvec))*75 
cvec <- round(cvec)

## create a col variable in the ctry5 dataset that 
## identifies a different gray saturation for each 
## value of GDP/capita.
ctry5$col <- paste0("gray", cvec)

## Panel (d): Colour Saturation

c5 <- ctry5 %>% arrange(gdp_cap) %>% 
  mutate(ctry_fac = factor(1:n(), labels=country))
ggplot(c5, aes(x=1, y=ctry_fac, colour=ctry_fac)) +
  geom_text(aes(label=ctry_fac), show.legend = FALSE) + 
  scale_colour_manual(values=c5$col) + 
  theme_void() 
# ggssave("output/f2_3d.png", height=4.5, width=4.5, dpi=300)
