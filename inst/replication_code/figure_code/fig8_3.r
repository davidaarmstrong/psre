## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(ggplot2)
library(car)
library(psre)
library(ggeffects)

## load data from psre package
data(wvs)

## estimate the non-linear transformation model
nlmod <- lm(moral ~ secpay + I(pct_high_rel_imp^3), data=wvs)

## save the coefficients from the model
b2 <- nlmod$coefficients

## identify a value of x to plot
xval <- .5
## derivative of y with respect to pct high religious importance
## slope of line tangent to the curve
sl <- 3*b2[3] *xval^2
## find value for y holding secpay 
## constant at 0.2084554
yval <- b2[1] + b2[2]*0.2084554 + I(xval^3)*b2[3]
## find intercept of line tangent to the curve
int <- yval -sl*xval
## make a data frame with both slope and intercept values
slint <- data.frame(slope = sl, intercept = int)

## do same as above, but with x=0.85
xval <- .85
sl <- 3*b2[3] *xval^2
yval <- b2[1] + b2[2]*0.2084554 + I(xval^3)*b2[3]
int <- yval -sl*xval
slint2 <- data.frame(slope = sl, intercept = int)

## Make the plot
ggplot() + 
  geom_line(data=g, aes(x=x, y=predicted)) + 
  geom_abline(aes(linetype="x=.5", colour="x=.5", slope=-0.1357112, intercept=0.3424685), size=1.2) + 
  geom_abline(aes(linetype="x=.85", colour="x=.85", slope=-0.3922055, intercept=0.519481), size=1.2) + 
  geom_point(aes(x=c(0.37, 0.63), y=c(0.288, 0.252))) + 
  geom_segment(aes(x=0.37, xend=0.63, y=0.252, yend=0.252), linetype=2) + 
  geom_segment(aes(x=0.37, xend=0.37, y=0.252, yend=0.288), linetype=2) + 
  geom_text(aes(x=0.5, y= 0.247, label="0.5 +/- 0.13")) + 
  geom_text(aes(x=0.33, y=0.27), label=expression(Delta~hat(y)), parse = TRUE) + 
  scale_linetype_manual(values=c(2,3)) + 
  scale_colour_manual(values=c("gray50", "black")) + 
  scale_x_continuous(breaks=c(.5, .85)) + 
  labs(x="Proportion for Whom Religion is Very Important", 
       y="Moral Permissiveness Scale", 
       linetype="", colour="") + 
  theme_classic() + 
  theme(legend.position=c(.2,.2), 
        legend.text = element_text(size=10))
ggsave("output/f8_3.png", height = 4.5, width=4.5, units="in", dpi=300)


### Average Marginal Effects





(3*.5^2)^2*0.0002822771

xval <- mean(wvs$pct_high_rel_imp, na.rm=TRUE)
yv1 <- b2[1] + b2[2]*0.2084554 + I((xval-.13)^3)*b2[3]
yv2 <- b2[1] + b2[2]*0.2084554 + I((xval+.13)^3)*b2[3]
xv1 <- xval-.13
xv2 <- xval+.13


B <- MASS::mvrnorm(2500, coef(nlmod), vcov(nlmod))
X <- rbind(c(1, 0.2084554, 0.54^3), 
           c(1, 0.2084554, 0.8^3))
# Average Difference
diff(X %*% coef(nlmod))

XB <- X%*% t(B)
delta1 <- apply(XB, 2, diff)
quantile(delta1, c(.025, .975))

X <- model.matrix(nlmod)
X[,3] <- X[,3]^(1/3)
X1 <- X2 <- X
X1[,3] <- (X1[,3] - .13)^3
X2[,3] <- (X2[,3] + .13)^3

mes1 <- X1 %*% t(B)
mes2 <- X2 %*% t(B)
delta <- mes2-mes1
cmd <- colMeans(delta)
mean(cmd)
quantile(cmd, c(.025, .975))
