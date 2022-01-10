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

## identify countries to include
ctries <- c("Australia","British Virgin Islands", "Bulgaria" ,
            "Colombia", "Comoros",  "Congo",  "Denmark",  "Egypt" ,
            "Swaziland" ,"Ethiopia", "Gabon", "Grenada" , "Lebanon", 
            "Lithuania" , "Mauritius","Mongolia", "Morocco",  "Nepal" ,
            "Peru" ,   "Philippines" , "Qatar", "Republic of Korea", 
            "Sri Lanka","Syrian Arab Republic", 
            "U.R. of Tanzania: Mainland")

## keep countries of interest for 2018 and arrange
## alphabetically
ctry_gdp <- inc_ineq %>% 
  filter(year == 2018) %>% 
  select(country, gdp_cap) %>% 
  filter(country %in% ctries) %>% 
  arrange(country) 

## Panel (a): Alphabetical Ordering, No Rounding
ctry_gdp %>% 
  setNames(c("Country", "GDP/capita"))

## Panel (b): Ordering and roudning
ctry_gdp %>% 
  arrange(-gdp_cap) %>% 
  mutate(gdp_cap = formatC(gdp_cap/1000, 
                           format="f", 
                           digits=0, 
                           big.mark=",")) %>% 
  setNames(c("Year", "GDP/capita")) 
