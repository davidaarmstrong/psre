## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## You must also install rgl, rpanel and misc3d from CRAN. 
## load packages
library(tidyverse)
library(psre)
library(sm)

## load data from psre package
data(wvs)

## remove values of resemaval <= .05 and sacsecval <=.05
wvs <- wvs %>% dplyr::filter(resemaval > 0.05 & sacsecval > 0.05 )

## make plot.
sm.density(wvs[,c("resemaval", "sacsecval", "moral")], 
           xlab="Emancipative Values", 
           ylab="Secular Values", 
           zlab="Moral Permissiveness")
