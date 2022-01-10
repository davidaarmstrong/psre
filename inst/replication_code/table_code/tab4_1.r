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
tab_4.1 <- table(gss$aidhouse, gss$party3)

## add marginal totals
tab_4.1 <- rbind(tab_4.1, colSums(tab_4.1))
rownames(tab_4.1)[5] <- "Total"
tab_4.1 <- cbind(tab_4.1, rowSums(tab_4.1))
colnames(tab_4.1)[4] <- "Total"

tab_4.1