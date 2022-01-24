## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages

library(tidyverse)
library(psre)
# library(reticulate)

## load data from psre package
data(wvs)

## filter out missing values on pct_mid_income
d2 <- wvs %>% 
  filter(!is.na(pct_mid_income))

## to make the Knuth and Scargle et al break points, you'll need
## to use reticulate and some python code.  I load the reticulate 
## package and point it to the appropriate python distribution on my
## computer.  You will need to change the path to the appropriate
## python distribution on you computer. 
reticulate::use_python("/Users/david/.pyenv/shims/")
# library(reticulate)
## You'll also need to install numpy and astropy.visualization
## for your python distribution.   

## newbreaks is a function that will calculate the appropriate
## break points based on the knuth or scargle (blocks) algorithm. 
newbreaks <- function(x, bins = c("knuth", "blocks")){
  bins = match.arg(bins)
  x <- na.omit(x)
  np <- reticulate::import("numpy")
  amp <- reticulate::import("astropy.visualization")
  l <- amp$hist(d2$pct_mid_income, bins)
  l[[2]]
}

## create a function to make break points according to 
## Scott's algorithm
scott_bins <- function(x){
  x <- na.omit(x)
  n <- length(x)
  floor((n^.33 * diff(range(x)))/(3.5*sd(x)))
}
## create a function to make break points according to 
## Friedman and Diaconis' algorithm
fd_bins <- function(x){
  x <- na.omit(x)
  n <- length(x)
  floor((n^.33 * diff(range(x)))/(2*IQR(x)))
}
## create vectors of break points for various methods. 
b.knuth <- newbreaks(d2$pct_mid_income, "knuth")
b.scargle <- newbreaks(d2$pct_mid_income, "blocks")
n <- nrow(d2)

b.fox <- floor(2*sqrt(n))
b.scott <- scott_bins(d2$pct_mid_income)
b.fd <- fd_bins(d2$pct_mid_income)

## create graphs 
ggplot(d2, aes(x=pct_mid_income)) + 
  geom_histogram(bins=b.scott, colour="white", size=.25) + 
  theme_classic()+ 
  labs(x="Proportion in Middle Income Category", 
       y="Frequency")
# ggssave("output/f3_3a.png", height=4.5, width=4.5, units="in", dpi=300)



ggplot(d2, aes(x=pct_mid_income)) + 
  geom_histogram(bins=b.fd, colour="white", size=.25) + 
  theme_classic()+ 
  labs(x="Proportion in Middle Income Category", 
       y="Frequency")
# ggssave("output/f3_3b.png", height=4.5, width=4.5, units="in", dpi=300)



ggplot(d2, aes(x=pct_mid_income)) + 
  geom_histogram(breaks = b.knuth, colour="white", size=.25) + 
  theme_classic()+ 
  labs(x="Proportion in Middle Income Category", 
       y="Frequency")
# ggssave("output/f3_3c.png", height=4.5, width=4.5, units="in", dpi=300)


ggplot(d2, aes(x=pct_mid_income)) + 
  geom_histogram(breaks = b.scargle, colour="white", size=.25) + 
  theme_classic()+ 
  labs(x="Proportion in Middle Income Category", 
       y="Frequency")
# ggssave("output/f3_3d.png", height=4.5, width=4.5, units="in", dpi=300)



## Panel (e): Fox 
ggplot(d2, aes(x=pct_mid_income)) + 
  geom_histogram(bins=b.fox, colour="white", size=.25) + 
  theme_classic() + 
  labs(x="Proportion in Middle Income Category", 
       y="Frequency")
# ggssave("output/f3_3e.png", height=4.5, width=4.5, units="in", dpi=300)

## Panel (f): Trial and Error
ggplot(d2, aes(x=pct_mid_income)) + 
  geom_histogram(bins=14, colour="white", size=.25) + 
  theme_classic()+ 
  labs(x="Proportion in Middle Income Category", 
       y="Frequency")
# ggssave("output/f3_3f.png", height=4.5, width=4.5, units="in", dpi=300)

