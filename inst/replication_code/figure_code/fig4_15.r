## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)

## load data from psre package
data(wvs)

## keep only the variables of interest in the investigation. 
tmp <- wvs %>% 
  select(resemaval, moral, pct_univ_degree, pct_female, pct_low_income, sacsecval) %>% 
  dplyr::filter(resemaval > 0 & moral > 0 & sacsecval > 0 ) %>%
  mutate(across(contains("pct"), ~.x*100))

## make the plot with the lsa() function from the psre package.
lsa(
  formula = as.formula(sacsecval ~ resemaval + moral + pct_univ_degree + 
                         pct_female + pct_low_income), 
  xlabels = c("Emancipative Vals", "Moral Perm", 
              "% Univ Degree", "% Female", "% Low Income"), 
  ylab = "Secular Values", 
  data=tmp)
ggsave("output/f4_15.png", height=3.5, width=12, dpi=300)
