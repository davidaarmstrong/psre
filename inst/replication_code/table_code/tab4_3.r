## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)

## load data from psre package
data(wvs)

## create pairwise correlation matrix
R <- DAMisc::pwCorrMat(wvs %>% 
                         ## select relevant variables
                         select(resemaval, moral, pct_univ_degree, 
                                pct_female, pct_low_income, sacsecval) %>% 
                         ## keep values bigger than zero for these three variables
                         filter(resemaval > 0 & moral > 0 & sacsecval > 0 ) %>% 
                         ## Set column names
                         setNames(., c("Emancipative\nValues", "Moral\nPermissiveness", 
                                  "% Univ Degree", "% Female", 
                                  "% Low Income", "Secular\nValues")))
R