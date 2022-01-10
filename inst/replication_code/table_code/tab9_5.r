## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)

## load data from psre package
data(ces)

## Set BQ and Green to "Other" and turn 
## gender into a factor
ces <- ces %>% 
  mutate(party3 = case_when(
    pid %in% c("BQ", "Green") ~ factor(6, 1:6, labels=levels(ces$pid)), 
    TRUE ~ pid), 
    party3 = droplevels(party3), 
    gender = factor(gender, labels=c("Male", "Female")))

## keep only those observations where 
## party ID is liberal or conservative
ces2 <- ces %>% filter(pid %in% c("Lib", "Con")) 

## interact each independent variable with party3 
## which has two levels - Liberal and Conservative
intmod3 <- lm(market ~ retrocan*party3 +
                gender*party3 + educ*party3, data=ces2)

## identify significant terms in interaction model
sigint <- ifelse(summary(intmod3)$coef[,4] < .05, "*", " ")

## estimate subset model for liberals
lib_mod <- lm(market ~ retrocan + gender + educ, 
              data=filter(ces2, pid == "Lib"))

## estimate subset model for conservatives
con_mod <- lm(market ~ retrocan + gender + educ, 
              data=filter(ces2, pid == "Con"))

## calculate the subset difference in coefficients
d <- coef(con_mod)- coef(lib_mod) 
## calculate the standard error of the difference. 
## this assumes no co-variance between liberal and 
## conservative parameters, which will be true in a 
## model where the subset variable has only 2 levels, 
## but not otherwise. 
sed <- sqrt(diag(vcov(lib_mod)) + diag(vcov(con_mod)))
## calculate the total residual degrees of freedom
resdf <- nobs(lib_mod) + nobs(con_mod) - lib_mod$rank - con_mod$rank

## calculate p-values for the difference
pd <- 2*pt(abs(d)/sed, df=resdf, lower.tail=FALSE)
## identify significant differences
sigd <- ifelse(pd < .05, "*", " ")
## identify significant coefficients in liberal 
## and conservative subset models
siglib <- ifelse(summary(lib_mod)$coef[,4] < .05, "*", " ")
sigcon <- ifelse(summary(con_mod)$coef[,4] < .05, "*", " ")

## print coefficients
cat(sprintf("%.2f%s\n(%.2f)", coef(intmod3), sigint, sqrt(diag(vcov(intmod3)))), sep="\n")

## Slight discrepancy - in the book, the intercept for the
## liberals in both models is -0.14, but in the table 
## below it is 0.13.

## combine results
mod9_c <- cbind(shuffle(coef(lib_mod), summary(lib_mod)$coef[,4], summary(lib_mod)$coef[,2], digits=2),  
      shuffle(coef(con_mod), summary(con_mod)$coef[,4], summary(con_mod)$coef[,2], digits=2), 
      shuffle(d, pd, sed, digits=2))
## identify the odd and even rows
odds <- seq(1, nrow(mod9_c), by=2)
evens <- seq(2, nrow(mod9_c), by=2)
## set all row names to ""
rownames(mod9_c) <- rep("", nrow(mod9_c))
## replace odd rownames with the coefficient names
rownames(mod9_c)[odds] <- names(coef(lib_mod))
## replace column names of result
colnames(mod9_c) <- c("Lib", "Cons", "Diff")
noquote(mod9_c)


## isolate the liberal simple slopes, standard errors
## and p-values from the interaction model
iblib <- coef(intmod3)[c(1:3, 5:7)]
iselib <- sqrt(diag(vcov(intmod3)))[c(1:3, 5:7)]
iplib <- 2*pt(abs(iblib)/iselib, df=intmod3$df.residual, lower.tail=FALSE)

## isolate the difference coefficients from 
## the interaction model
ibdif <- coef(intmod3)[c(4,8:12)]
isedif <- sqrt(diag(vcov(intmod3)))[c(4,8:12)]
ipdif <- 2*pt(abs(ibdif)/isedif, df=intmod3$df.residual, lower.tail=FALSE)


## generate a matrix, A, that will be 
## used to calculate the simple slopes
## standard errors and p-values for 
## conservatives. 
A <- matrix(0, 
            ncol=length(coef(intmod3)), 
            nrow = 6)
colnames(A) <- names(coef(intmod3))
A[cbind(1, c(1,4))] <- 1
A[cbind(2, c(2,8))] <- 1
A[cbind(3, c(3,9))] <- 1
A[cbind(4, c(5,10))] <- 1
A[cbind(5, c(6,11))] <- 1
A[cbind(6, c(7,12))] <- 1

## calculate simple slopes, standard errors
## and p-values for conservatives from the 
## interaction model
ibcon <- A %*% coef(intmod3)
isecon <- sqrt(diag(A %*% vcov(intmod3) %*% t(A)))
ipcon <-  2*pt(abs(ibcon)/isecon, df=intmod3$df.residual, lower.tail=FALSE)

## put results together as above. 
mod9_d <- cbind(shuffle(iblib, iplib, iselib, digits=2),  
                shuffle(ibcon, ipcon, isecon, digits=2),  
                shuffle(ibdif, ipdif, isedif, digits=2))
odds <- seq(1, nrow(mod9_d), by=2)
evens <- seq(2, nrow(mod9_d), by=2)
rownames(mod9_d) <- rep("", nrow(mod9_d))
rownames(mod9_d)[odds] <- names(coef(lib_mod))
colnames(mod9_d) <- c("Lib", "Cons", "Diff")
noquote(mod9_d)

noquote(cbind(mod9_c, mod9_d))