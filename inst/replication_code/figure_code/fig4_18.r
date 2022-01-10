## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(sm)

## load data from psre package
data(wvs)

## remove values of resemaval <= .05 and sacsecval <=.05
wvs <- wvs %>% dplyr::filter(resemaval > 0.05 & sacsecval > 0.05 )

## A. Perspective Plot
png("output/f4_18a.png", height=4.5, width=4.5, units="in", res=300)
sm.density(wvs[,c("resemaval", "sacsecval")], 
           xlab="Emancipative Values", 
           ylab="Secular Values", 
           zlab="Density", 
           col="gray75")
dev.off()

## B. Image plot
png("output/f4_18b.png", height=4.5, width=4.5, units="in", res=300)
sm.density(wvs[,c("resemaval", "sacsecval")], 
           xlab="Emancipative Values", 
           ylab="Secular Values", 
           zlab="Density", 
           col="gray75", display="image")
dev.off()

## C. Contour Plot
png("output/f4_18c.png", height=4.5, width=4.5, units="in", res=300)
sm.density(wvs[,c("resemaval", "sacsecval")], 
           xlab="Emancipative Values", 
           ylab="Secular Values", 
           zlab="Density", 
           col="gray75", display="slice")
dev.off()
