## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(ggeffects)
library(statmod)

## set random number generating seed
set.seed(202)

## make a variance-covariance 
## matrix that has 1 on the 
## diagonal and .5 on the off-
## diagonal
sig <- matrix(.5, ncol=2, nrow=2)
diag(sig) <- 1

## Draw two x variables from a 
## multivariate normal distribution
x <- MASS::mvrnorm(1000, c(0,0), sig)
## make the design matrix that includes
## x and x squared
X <- cbind(1, x[,1], x[,2], x[,2]^2)
## make coefficients 
beta <- c(-1,  .5, -.25, .25)
## make linear predictor
xb <- X %*% beta
## generate mu as e^xb
mu <- exp(xb)
## Draw y from a random poisson distribution
## with the defined mu
y <- rpois(1000, mu)
## make hypothetical data with y that 
## is a function of x and x-squared 
## along with x1 and x2.
hdat1 <- data.frame(
  y = y, 
  x1 = x[,1],
  x2 = x[,2]
)

## estimate the mis-specified model
h1 <- glm(y ~ x1 + x2, data=hdat1, family=poisson)

## estimate the properly specified 
## model that includes x-squared
h1_true <- glm(y ~ x1 + poly(x2, 2), data=hdat1, family=poisson)


## proceed as above, but in this case 
## the mis-specification is monotone, 
## simple non-linearity in x2

set.seed(519)
sig <- matrix(.5, ncol=2, nrow=2)
diag(sig) <- 1
x1 <- rnorm(1000, 0, 1)
x2 <- runif(1000, 10, 1000)
X <- cbind(1, x1, log(x2))
beta <- c(-2, .25, .5)
xb <- X %*% beta
mu <- exp(xb)
y <- rpois(1000, mu)
hdat2 <- data.frame(
  y = y, 
  x1 = x1,
  x2 = x2
)

h2 <- glm(y ~ x1 + x2, data=hdat2, family=poisson)
h2_true <- glm(y ~ x1 + log(x2), data=hdat2, family=poisson)

## proceed as above, but this time, the 
## mis-specification is that x2 is 
## nominal and we treat it as quantitative.

set.seed(519)
sig <- matrix(.5, ncol=2, nrow=2)
diag(sig) <- 1
x1 <- rnorm(1000, 0, 1)
x2 <- sample(1:5, 1000, replace=TRUE)
X <- cbind(x1, x2)
X <- as.data.frame(X)
X$x2 <- as.factor(X$x2)
Xmat <- model.matrix(~., data=X)
beta <- c(0, .25, .5, -.5, 1, -1)
xb <- Xmat %*% beta
mu <- exp(xb)
y <- rpois(1000, mu)
hdat3 <- data.frame(
  y = y, 
  x1 = X$x1,
  x2 = x2
)

h3 <- glm(y ~ x1 + x2, data=hdat3, family=poisson)
h3_true <- hdat3 %>% mutate(x2fac = as.factor(x2)) %>% 
  glm(y ~ x1 + x2fac, data=., family=poisson, x=TRUE, y = TRUE)


## get quantile-quantile plot data 
set.seed(301)
qqdf <- qqPoints(qresid(h1))
a <- attr(qqdf, "ab")[1]
b <- attr(qqdf, "ab")[2]
l <- min(qqdf$theo) * b + a
u <- max(qqdf$theo) * b + a

## A. Quadratic (poisson)
ggplot(qqdf, aes(x=theo, y=x)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15) + 
  geom_segment(aes(x=min(qqdf$theo), xend=max(qqdf$theo), y = l, yend=u)) + 
  geom_point(shape=1) + 
  theme_classic() + 
  labs(x="Theoretical Quantiles", 
       y="Observed Quantiles")
ggsave("output/f10_13a.png", height=4.5, width=4.5, units="in", dpi=300)


## get quantile-quantile data
qqdf <- qqPoints(qresid(h2))
a <- attr(qqdf, "ab")[1]
b <- attr(qqdf, "ab")[2]
l <- min(qqdf$theo) * b + a
u <- max(qqdf$theo) * b + a

## C. Simple, monotone (poisson)
ggplot(qqdf, aes(x=theo, y=x)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15) + 
  geom_segment(aes(x=min(qqdf$theo), xend=max(qqdf$theo), y = l, yend=u)) + 
  geom_point(shape=1) + 
  theme_classic() + 
  labs(x="Theoretical Quantiles", 
       y="Observed Quantiles")
ggsave("output/f10_13c.png", height=4.5, width=4.5, units="in", dpi=300)


## get quantile-quantile data
qqdf <- qqPoints(qresid(h3))
a <- attr(qqdf, "ab")[1]
b <- attr(qqdf, "ab")[2]
l <- min(qqdf$theo) * b + a
u <- max(qqdf$theo) * b + a

## E. Categorical (poisson)
ggplot(qqdf, aes(x=theo, y=x)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15) + 
  geom_segment(aes(x=min(qqdf$theo), xend=max(qqdf$theo), y = l, yend=u)) + 
  geom_point(shape=1) + 
  theme_classic() + 
  labs(x="Theoretical Quantiles", 
       y="Observed Quantiles")
ggsave("output/f10_13e.png", height=4.5, width=4.5, units="in", dpi=300)


## Estimate negative binomial models
h1n <- MASS::glm.nb(y ~ x1 + x2, data=hdat1)
h2n <- MASS::glm.nb(y ~ x1 + x2, data=hdat2)
h3n <- MASS::glm.nb(y ~ x1 + x2, data=hdat3)

## B. Quadratic (negative binomial)
set.seed(301)
qqdf <- qqPoints(qresid(h1n))
a <- attr(qqdf, "ab")[1]
b <- attr(qqdf, "ab")[2]
l <- min(qqdf$theo) * b + a
u <- max(qqdf$theo) * b + a


ggplot(qqdf, aes(x=theo, y=x)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15) + 
  geom_segment(aes(x=min(qqdf$theo), xend=max(qqdf$theo), y = l, yend=u)) + 
  geom_point(shape=1) + 
  theme_classic() + 
  labs(x="Theoretical Quantiles", 
       y="Observed Quantiles")
ggsave("output/f10_13b.png", height=4.5, width=4.5, units="in", dpi=300)

## D. Simple, monotone (negative binomial)
qqdf <- qqPoints(qresid(h2n))
a <- attr(qqdf, "ab")[1]
b <- attr(qqdf, "ab")[2]
l <- min(qqdf$theo) * b + a
u <- max(qqdf$theo) * b + a


ggplot(qqdf, aes(x=theo, y=x)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15) + 
  geom_segment(aes(x=min(qqdf$theo), xend=max(qqdf$theo), y = l, yend=u)) + 
  geom_point(shape=1) + 
  theme_classic() + 
  labs(x="Theoretical Quantiles", 
       y="Observed Quantiles")
ggsave("output/f10_13d.png", height=4.5, width=4.5, units="in", dpi=300)

## F. Categorical (negative binomial)
qqdf <- qqPoints(qresid(h3n))
a <- attr(qqdf, "ab")[1]
b <- attr(qqdf, "ab")[2]
l <- min(qqdf$theo) * b + a
u <- max(qqdf$theo) * b + a

ggplot(qqdf, aes(x=theo, y=x)) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15) + 
  geom_segment(aes(x=min(qqdf$theo), xend=max(qqdf$theo), y = l, yend=u)) + 
  geom_point(shape=1) + 
  theme_classic() + 
  labs(x="Theoretical Quantiles", 
       y="Observed Quantiles")
ggsave("output/f10_13f.png", height=4.5, width=4.5, units="in", dpi=300)




