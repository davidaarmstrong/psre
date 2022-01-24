## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(ggrepel)

## set random number generating seed
set.seed(202)

## Make data that we can use to demonstrate interaction effects.
dat <- data.frame(
  x= sample(1:10, 250, replace=TRUE), 
  d = sample(c(0,1), 250, replace=TRUE))

## make a design matrix from the hypothetical data
## This is for an interaction between a continuous
## variable and a dummy variable.
X <- with(dat, cbind(1, x, d, x*d))
b <- c(1, .02, 1, -.2)
## make the dependent variable 
dat$y <- c(X%*% b) + rnorm(250, 0, sd=.75)
## estimate the model
m <- lm(y ~ x*d, data=dat)
## generate data that can be used 
## for prediction 
tmpdat <- expand.grid(x=1:10, d=0:1, y=0)

## get the design matrix from the model
newX <- model.matrix(formula(m), data=tmpdat)
## get the variance-covariance matrix from the model
V <- vcov(m)
## get the coefficients from the model
bmod <- coef(m)

## Generate predictions and confidence intervals for
## the predictions from the model
plot_dat <- tibble(
  x = tmpdat$x, 
  d = tmpdat$d, 
  predicted = c(newX %*% bmod),
  s = sqrt(diag(newX %*% V %*% t(newX))), 
  conf.low = predicted - qt(.975, m$df.residual)*s, 
  conf.high = predicted + qt(.975, m$df.residual)*s, 
  dlab = case_when(x == 10 ~ paste0("d = ", d), TRUE ~ NA_character_), 
  xlab = case_when(d == 1 ~ paste0("x = ", x), TRUE ~ NA_character_))


## B. Quantitative variable by categorical variable
f1 <- ggplot(plot_dat, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high, fill=as.factor(d))) + 
  geom_ribbon(alpha=.25, show.legend=FALSE) + 
  geom_line(aes(linetype=as.factor(d)), show.legend = FALSE) + 
  geom_text_repel(aes(label=dlab), vjust=.5, hjust=1) + 
  scale_fill_manual(values=c("gray50", "gray50"))+
  scale_linetype_manual(values=c(2,1)) + 
  labs(x="X", y="Fitted Values", linetype="d") + 
  theme_classic() + 
  theme(legend.position="top") + 
  coord_cartesian(xlim=c(1, 11), expand=TRUE) 
f1 
# ggssave("output/f9_1b.png", height=4.5, width=4.5, units="in", dpi=300)

## get coefficients from the model
b <- coef(m)
## intercept for d=0
b0nd <- b[1]
## intercept for d=1
b0ed<- b[1] + b[3]
## slope for d=0
b1nd <- b[2]
## slope for d=1
b1ed <- b[2] + b[4]


# 
# plot_dat <- tibble(
#   x = tmpdat$x, 
#   d = tmpdat$d, 
#   predicted = c(newX %*% bmod),
#   s = sqrt(diag(newX %*% V %*% t(newX))), 
#   conf.low = predicted - qt(.975, m$df.residual)*s, 
#   conf.high = predicted + qt(.975, m$df.residual)*s, 
#   dlab = case_when(x == 10 ~ paste0("d = ", d), TRUE ~ NA_character_), 
#   xlab = case_when(d == 1 ~ paste0("x = ", x), TRUE ~ NA_character_))
# 

## get range of y-axis for previous plot. 
f1ab <- ggplot_build(f1)
rgy <- f1ab$layout$panel_params[[1]]$y.range

## build a dataset that has the various 
## slopes and intercepts in it. 
line_dat <- tibble(
  x = rep(c(0,10), 2),
  d = factor(c(1,1,2,2), labels=c("0", "1")), 
  slope = c(b1nd, b1nd, b1ed, b1ed), 
  int = c(b0nd, b0nd, b0ed, b0ed), 
  yhat = int + x*slope
)

## A. Understanding the Coefficients

ggplot() + 
  geom_point(data=tmpdat, aes(x=x, y=y), col="transparent") + 
  geom_line(data=line_dat, aes(x=x, y=yhat, linetype=as.factor(d), colour=as.factor(d)))+ 
  ylim(rgy) + 
  geom_segment(aes(x=3, y=plot_dat$predicted[3], xend=4, yend=plot_dat$predicted[3]), 
               arrow=arrow(length=unit(.175, "cm"), ends="last")) + 
  geom_segment(aes(x=4, y=plot_dat$predicted[3], xend=4, yend=plot_dat$predicted[4]), 
               arrow=arrow(length=unit(.175, "cm"), ends="last")) + 
  geom_segment(aes(x=3, y=plot_dat$predicted[13], xend=4, yend=plot_dat$predicted[13]), 
               arrow=arrow(length=unit(.175, "cm"), ends="last")) + 
  geom_segment(aes(x=4, y=plot_dat$predicted[13], xend=4, yend=plot_dat$predicted[14]), 
               arrow=arrow(length=unit(.175, "cm"), ends="last")) + 
  geom_text(aes(x=3.5, y=.86, label="1")) +
  geom_text(aes(x=4.15, y=mean(plot_dat$predicted[3:4]), label="b[1]"), parse=TRUE, hjust=0) +
  geom_text(aes(x=3.5, y=1.4, label="1")) +
  geom_text(aes(x=4.15, y=mean(plot_dat$predicted[13:14]), label="b[1]+b[3]"), parse=TRUE, hjust=0) +
  geom_text(aes(x=0, y=b[1]-.005, label="b[0]"), parse=TRUE, hjust=0, vjust=1) +
  geom_text(aes(x=0, y=b[1]+b[3]+.005, label="b[0]+b[2]"), parse=TRUE, hjust=0, vjust=0) +
  geom_segment(aes(x=0, y=b[1]+.01, xend=0, yend=(b[1]+b[3])-.01), 
               arrow=arrow(length=unit(.175, "cm"), ends="both")) + 
  geom_text(aes(x=0.15, y=(b[1]+.5*b[3]), label="b[2]"), hjust=0, parse=TRUE)+
  scale_colour_manual(values=c("black", "gray50"))+ 
  scale_linetype_manual(values=c(2,1)) + 
  theme_classic() + 
  theme(legend.position=c(.8,.9)) + 
  labs(x="x", 
       y="y", 
       colour="d", linetype="d") 
# ggssave("output/f9_1a.png", height=4.5, width=4.5, units="in", dpi=300)



## C. Categorical variable by Quantitative variable
ggplot(plot_dat, aes(x=as.factor(d), y=predicted, ymin=conf.low, ymax=conf.high)) + 
  geom_errorbar( width=.1) + 
  geom_point(size=.5) + 
  geom_line(aes(group=1)) + 
  facet_wrap(~factor(x, labels=paste0("x=", 1:10))) + 
  labs(x="d", y="Fitted Values", linetype="d") + 
  #scale_x_continuous(breaks=c(0,1)) + 
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        legend.position="top")
# ggssave("output/f9_1c.png", height=4.5, width=4.5, units="in", dpi=300)


## make hypothetical data for the two quantitative-variable
## interaction model. 
set.seed(202)
dat <- data.frame(
  x1= sample(1:10, 1000, replace=TRUE), 
  x2= sample(1:10, 1000, replace=TRUE))
X <- with(dat, cbind(1, x1, x2, x1*x2))
## set the coefficients
b <- c(1, .08, .01, -.02)
## generate y
dat$y <- c(X%*% b) + rnorm(250, 0, sd=.35)
## estimate the model
m <- lm(y ~ x1*x2, data=dat)
## make data that will be used to make predictions
tmpdat <- expand.grid(x1=1:10, x2=1:10, y=0)

## Make the design matrix
newX <- model.matrix(formula(m), data=tmpdat)
## get variance-covariance matrix from the model
V <- vcov(m)
## get coefficients from the model
bmod <- coef(m)

## make the data that will be used in the figure
plot_dat <- tibble(
  x1 = tmpdat$x1, 
  x2 = tmpdat$x2, 
  predicted = c(newX %*% bmod),
  s = sqrt(diag(newX %*% V %*% t(newX))), 
  conf.low = predicted - qt(.975, m$df.residual)*s, 
  conf.high = predicted + qt(.975, m$df.residual)*s, 
  x2lab = case_when(x1 == 10 ~ paste0("x2=", x2), TRUE ~ NA_character_), 
  x1lab = case_when(x2 == 10 ~ paste0("x1=", x1), TRUE ~ NA_character_))

## D. Two quantitative variables.
ggplot(plot_dat, aes(x=x2, y=predicted, ymin=conf.low, 
                     ymax=conf.high, group=as.factor(x1))) + 
  geom_line() + 
  geom_text_repel(aes(label=x1lab), vjust=.5, hjust=1) + 
  theme_classic() + 
  coord_cartesian(xlim=c(1,11), expand=TRUE)+ 
  labs(x= "X2", y="Fitted Values")
# ggssave("output/f9_1d.png", height=4.5, width=4.5, units="in", dpi=300)


