## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(ggeffects)

## load data from psre package
data(wvs)

## keep only those countries where there are some 
## people with a university degree
wvs2 <- wvs %>% filter(pct_univ_degree > 0)

## estimate model
intmod4 <- lm(resemaval ~ gini_disp*pct_high_rel_imp, data=wvs)

## create a function that will make predictions given the 
## input of two values - gini coefficient (x) and 
## religious importance (y)

myfun <- function(x,y){
  a <- c(1, x, y, x*y)
  c(a %*% coef(intmod4))
}

## make sequences of values for gini and religious importance
## that vary across their ranges. 
s.gini <- with(wvs, seq(min(gini_disp, na.rm=TRUE), 
                        max(gini_disp, na.rm=TRUE), length=25))
s.ri<- with(wvs, seq(min(pct_high_rel_imp, na.rm=TRUE), 
                     max(pct_high_rel_imp, na.rm=TRUE), length=26))

## make predictions using the combinations of values in each 
## of the sequences for gini and religious importance 
## This makes a 25x26 matrix
o <- outer(s.gini, s.ri, Vectorize(myfun))

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

## A. Left origin
png("output/f9_7a.png", height=5.5, width=5.5, units="in", res=300)
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

## B. Right origin
theta <- 215
png("output/f9_7b.png", height=5.5, width=5.5, units="in", res=300)
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


