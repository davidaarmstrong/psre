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

tab2.1a <- us %>% 
  ## select relevant variables, 
  select(year, gdp_cap, gini) %>% 
  ## format dollars appropriately and round gini
  mutate(gdp_cap = paste0("$", formatC(gdp_cap, format="f", big.mark=",",
                                       digits=0)), 
         gini = round(gini, 3)) %>% 
  ## set column names of table
  setNames(c("Year", "GDP/capita", "Gini"))

## print table
tab2.1a
