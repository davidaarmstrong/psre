## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(corrplot)

## load data from psre package
data(wvs)

## make correlation matrix among a subset of continuous variables
R <- cor(wvs %>% 
           select(resemaval, moral, pct_univ_degree, pct_female, pct_low_income, sacsecval) %>% 
           dplyr::filter(resemaval > 0 & moral > 0 & sacsecval > 0 ) %>% 
           setNames(., c("Emancipative\nValues", "Moral\nPermissiveness", 
                         "% Univ Degree", "% Female", 
                         "% Low Income", "Secular\nValues")), use="pair")

## Find the order of the correlations from largest to smallest.  
ord <- order(R[1, ])
## reorder the correlation matrix based on the correlations with the first variable
R_ord <- R[ord, ord]
## crate a gray-scale color ramp
gray_pal <- colorRampPalette(c("gray10", "gray90"))


## open a png driver to save the plot
png("output/f4_13a.png", height=4.5, width=5, units="in", res=300)
corrplot(R_ord, 
         method="ellipse", 
         tl.col="black", 
         col=gray_pal(200))
dev.off()

## Create correlation matrix of variables using a BCn transformation 
## this transformation is executing using the trans_fun function from 
## the psre package, which is not exported from the namespace, thus
## the ::: to extract it.  This is just a wrapper around powerTransform
## from the car package.
R2 <- cor(wvs %>% 
            select(resemaval, moral, pct_univ_degree, pct_female, pct_low_income, sacsecval) %>% 
            dplyr::filter(resemaval > 0 & moral > 0 & sacsecval > 0 ) %>% 
            setNames(., c("Emancipative\nValues", "Moral\nPermissiveness", 
                          "% Univ Degree", "% Female", 
                          "% Low Income", "Secular\nValues")) %>% 
            mutate(across(everything(), psre:::trans_fun)), use="pair")

## proceed as above. 

R2_ord <- R2[ord, ord]
png("output/f4_13b.png", height=4.5, width=5, units="in", res=300)
corrplot(R2_ord, 
         method="ellipse", 
         tl.col="black", 
         col=gray_pal(200))
dev.off()

## keep only the variables of interest in the investigation. 
tmpa <- wvs %>% 
  select(resemaval, moral, pct_univ_degree, pct_female, pct_low_income, sacsecval) %>% 
  dplyr::filter(resemaval > 0 & moral > 0 & sacsecval > 0 ) %>% 
  setNames(., c("Emancipative\nValues", "Moral\nPermissiveness", 
                "% Univ Degree", "% Female", 
                "% Low Income", "Secular\nValues"))

## create a correlation matrix that is the R-squared of a loess fit
## with the first index as the dependent variable and the 
## second index as independent variable. 
R3 <- outer(1:6, 1:6, Vectorize(function(x,y)assocfun(x,y,tmpa)))
rownames(R3) <- colnames(R3) <- rownames(R2)
R3 <- R3 * sign(R2)
R3_ord <- R3[ord, ord]
png("output/f4_13c.png", height=4.5, width=5, units="in", res=300)
corrplot(R3_ord, 
         method="ellipse", 
         tl.col="black", 
         col=gray_pal(200))
dev.off()
