## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)

## load data from psre package
data(gss)

## filter to only those observations that have valid 
## data on party3 and aidhouse
gss <- gss %>% filter(!is.na(party3) & !is.na(aidhouse))

## make cross-tabulation
tab_4.2 <- table(gss$aidhouse, gss$party3)

## turn table into column proportions
tab_4.2 <- round(prop.table(tab_4.2, 2)*100)
## make first column add to 100
tab_4.2[4,1] <- 3
## add column marginals
tab_4.2 <- rbind(tab_4.2, colSums(tab_4.2))
rownames(tab_4.2)[5] <- "Total"

tab_4.2