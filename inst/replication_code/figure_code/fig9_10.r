## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(mgcv)

## load data from psre package
data(wvs)

## Select relevant variables
wvs2 <- wvs %>% 
  dplyr::select(country, year, resemaval, 
                pct_high_rel_imp, gini_disp, democrat) %>% 
  na.omit()

## estimate non-linear interaction
m2b <- gam(resemaval ~ te(pct_high_rel_imp) + te(gini_disp) + ti(pct_high_rel_imp, gini_disp), data= wvs2)
## estimate linear interaction 
m2brest <- gam(resemaval ~ pct_high_rel_imp*gini_disp, data= wvs2)

## function that produces a prediction based on two input values: 
## gini (x) and religious importance (y)
myfun2 <- function(x,y){
  tmp <- data.frame(
    gini_disp = x,
    pct_high_rel_imp = y, 
    resemaval=.5)
  c(predict(m2b, newdata=tmp))
}

## make sequences of values from the minimum to the maximum of
## both gini and religious importance
s.gini <- with(wvs, seq(min(gini_disp, na.rm=TRUE), 
                        max(gini_disp, na.rm=TRUE), length=25))
s.ri<- with(wvs, seq(min(pct_high_rel_imp, na.rm=TRUE), 
                     max(pct_high_rel_imp, na.rm=TRUE), length=26))

## generate predictions, o will be a 25 x 26 matrix
o <- outer(s.gini, s.ri, Vectorize(myfun2))
o[which(o < 0, arr.ind=TRUE)] <- NA

## set some global parameters
## the predicted surface is our prediction 
## v1.seq is the set of gini values
## v2.seq is the set of religious importance values. 
predsurf <- o
v1.seq <- s.gini
v2.seq <- s.ri
## hcols is a sequence of length 4 giving gray-scale colours
hcols <- paste("gray", seq(from = 20, to = 80, length = 4),
               sep = "")
## set axis labels for the graph
xlab <- "Gini"
ylab <- "Religious Importance"
zlab <- "Emancipative Values"
## dtmp is the original listwise deleted data. 
dtmp <- na.omit(wvs[, c("gini_disp", "pct_high_rel_imp")])
## dens is the bivariate density of the gini and religious
## importance variables. 
dens <- MASS::kde2d(dtmp[,1], dtmp[,2])
## theta and phi give the angles of rotation of the plot
## around the vertical and horizontal axes
theta <- 35
phi <- 15
## identify cutoffs for the density that will identify 
## which regions get shaded which colorus. 
cutoff <- quantile(c(dens$z), prob = c(0.25, 0.5,
                                       0.75))
## set the predictions to NA if they are not in the 
## highest density regions as defined by the 
## cutoffs calculated above. 
pred1 <- predsurf
pred1[dens$z < cutoff[1]] <- NA
pred2 <- predsurf
pred2[dens$z < cutoff[2]] <- NA
pred3 <- predsurf
pred3[dens$z < cutoff[3]] <- NA

## A. Left Origin
png("output/f9_10a.png", height=5.5, width=5.5, units="in", res=300)
persp(v1.seq, v2.seq, predsurf,
      zlim=c(0, 0.71), 
      xlab = ifelse(is.null(xlab), toupper(v1), xlab),
      ylab = ifelse(is.null(ylab), toupper(v2), ylab),
      zlab = ifelse(is.null(zlab), toupper("Predictions"), zlab),
      col = hcols[1], theta = theta, phi = phi, ticktype="detail")
par(new = TRUE)
persp(v1.seq, v2.seq, pred1, col = hcols[2], axes = FALSE,
      zlim=c(0, 0.71), 
      xlab = "", ylab = "", zlab = "", theta = theta, phi = phi,
      #        zlim = c(min(c(predsurf)), max(c(predsurf))), 
      ylim = c(min(v2.seq),max(v2.seq)), 
      xlim = c(min(v1.seq), max(v1.seq)))
par(new = TRUE)
persp(v1.seq, v2.seq, pred2, col = hcols[3], axes = FALSE,
      zlim=c(0, 0.71), 
      xlab = "", ylab = "", zlab = "", theta = theta, phi = phi,
      #        zlim = c(min(c(predsurf)), max(c(predsurf))), 
      ylim = c(min(v2.seq), max(v2.seq)), 
      xlim = c(min(v1.seq), max(v1.seq)))
par(new = TRUE)
persp(v1.seq, v2.seq, pred3, col = hcols[4], axes = FALSE,
      zlim=c(0, 0.71), 
      xlab = "", ylab = "", zlab = "", theta = theta, phi = phi,
      #        zlim = c(min(c(predsurf)), max(c(predsurf))), 
      ylim = c(min(v2.seq), max(v2.seq)), 
      xlim = c(min(v1.seq), max(v1.seq)))
dev.off()

## B. Right Origin
theta <- 215
png("output/f9_10b.png", height=5.5, width=5.5, units="in", res=300)
persp(v1.seq, v2.seq, predsurf,
      zlim=c(0, 0.71), 
      xlab = ifelse(is.null(xlab), toupper(v1), xlab),
      ylab = ifelse(is.null(ylab), toupper(v2), ylab),
      zlab = ifelse(is.null(zlab), toupper("Predictions"), zlab),
      col = hcols[1], theta = theta, phi = phi, ticktype="detail")
par(new = TRUE)
persp(v1.seq, v2.seq, pred1, col = hcols[2], axes = FALSE,
      zlim=c(0, 0.71), 
      xlab = "", ylab = "", zlab = "", theta = theta, phi = phi,
      #      zlim = c(min(c(predsurf)), max(c(predsurf))), 
      ylim = c(min(v2.seq),max(v2.seq)), 
      xlim = c(min(v1.seq), max(v1.seq)))
par(new = TRUE)
persp(v1.seq, v2.seq, pred2, col = hcols[3], axes = FALSE,
      zlim=c(0, 0.71), 
      xlab = "", ylab = "", zlab = "", theta = theta, phi = phi,
      #      zlim = c(min(c(predsurf)), max(c(predsurf))), 
      ylim = c(min(v2.seq),max(v2.seq)), 
      xlim = c(min(v1.seq), max(v1.seq)))
par(new = TRUE)
persp(v1.seq, v2.seq, pred3, col = hcols[4], axes = FALSE,
      zlim=c(0, 0.71), 
      xlab = "", ylab = "", zlab = "", theta = theta, phi = phi,
      #      zlim = c(min(c(predsurf)), max(c(predsurf))), 
      ylim = c(min(v2.seq), max(v2.seq)), 
      xlim = c(min(v1.seq), max(v1.seq)))
dev.off()


#####################

## We didn't make these graphs in the book, but we did suggest
## that we tested to see whether there was significant 
## non-linearity.  Here is how we figured that out. 

## reduce civ to 3 categories and make a new dummy regressor for 
## each one. 
wvs1 <- wvs %>% 
  mutate(
    civ2 = case_when(
      civ == 9 ~ "Western", 
      civ == 6 ~ "Latin American", 
      TRUE ~ "Other"),
    civ2 = factor(civ2, levels=c("Western", "Latin American", "Other")), 
    democrat= factor(democrat, levels=1:2, 
                     labels=c("New Democracy", "Established Democracy")), 
    civw = as.numeric(civ2 == "Western"), 
    civl = as.numeric(civ2 == "Latin American"), 
    civo = as.numeric(civ2 == "Other"))


## estimate generalized additive model with a different smooth for each value
## of the civilization variable
m1 <- gam(resemaval ~ s(pct_secondary, by=civw) + s(pct_secondary, by=civl) + 
            s(pct_secondary, by=civo) + civ2, data=wvs1)
## estimate the linear interaction model. 
m1rest <- gam(resemaval ~ civ2 * pct_secondary, data=wvs1)
## Test the two models against each other.  A significant 
## F statistic indicates that a non-linear interaction is
## a better fit to the data. 
anova(m1, m1rest,test="F")


