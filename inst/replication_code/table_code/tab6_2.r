## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(psre)

## load data from psre package
data(wvs)

## Civilization labels
codes <- c("Other", "African", "Buddhist", "Hindu", "Islamic", "Japanese", 
           "Latin American", "Orthodox", "Sinic", "Western")

## recode civilization categories
wvs <- wvs %>% 
  mutate(civ = case_when(
    civ %in% c(0,1,2,3,5) ~ "Other", 
    civ == 5 ~ "Islamic", 
    civ == 7 ~ "Latin American", 
    civ == 8 ~ "Sinic", 
    civ == 9 ~ "Western"), 
    civ = factor(civ, levels=c("Western", "Sinic", "Islamic", "Latin American", "Other")))

## set contrasts to effect coding
contrasts(wvs$civ) <- contr.sum(c("Sinic", "Islamic", "Latin American", 
                                  "Other", "Western"))
contrasts(wvs$civ)