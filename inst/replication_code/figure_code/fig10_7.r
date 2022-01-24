## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(ggeffects)

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

## get predictions for the relevant x from the 
## mis-specified model. 
e1 <- ggpredict(h1, terms="x2 [n=50]") %>% mutate(model = factor(1, levels=1:2, labels=c("Misspecified", "True")))
## get predictions for the relevant x from the 
## appropriately specified model. 
e1t <- ggpredict(h1_true, terms="x2 [n=50]") %>% mutate(model = factor(2, levels=1:2, labels=c("Misspecified", "True")))
## put both predictions together 
e1 <- bind_rows(e1, e1t)

## A. Quadratic
ggplot(e1, aes(x=x, y=predicted, ymin = conf.low, 
               ymax=conf.high, linetype=model)) + 
  geom_ribbon(alpha=.25, show.legend=FALSE) + 
  geom_line() + 
  theme_classic() + 
  theme(legend.position = c(.85, .85)) + 
  labs("x2", y="E(Y|b,X)", linetype="") 
# ggssave("output/f10_7a.png", height=4.5, width=4.5, units="in", dpi=300)


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
x2s <- seq(min(hdat2$x2), max(hdat2$x2), length=50)
e2 <- ggpredict(h2, terms="x2 [x2s]") %>% mutate(model = factor(1, levels=1:2, labels=c("Misspecified", "True")))
e2t <- ggpredict(h2_true, terms="x2 [x2s]") %>% mutate(model = factor(2, levels=1:2, labels=c("Misspecified", "True")))
e2 <- bind_rows(e2, e2t)

## B. Monotone
ggplot(e2, aes(x=x, y=predicted, ymin = conf.low, 
               ymax=conf.high, linetype=model)) + 
  geom_ribbon(alpha=.25, show.legend=FALSE) + 
  geom_line() + 
  theme_classic() + 
  theme(legend.position = c(.2, .9)) + 
  labs("x2", y="E(Y|b,X)", linetype="") 
# ggssave("output/f10_7b.png", height=4.5, width=4.5, units="in", dpi=300)


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

e3 <- ggpredict(h3, terms="x2 [all]") %>% mutate(model = factor(1, levels=1:2, labels=c("Misspecified", "True")))
e3t <- ggpredict(h3_true, terms="x2fac") %>% mutate(
  x = as.numeric(x), 
  model = factor(2, levels=1:2, labels=c("Misspecified", "True")))
e3 <- bind_rows(e3, e3t)

## C. Categorical 
ggplot(e3, aes(x=x, y=predicted, ymin = conf.low, 
               ymax=conf.high, linetype=model)) + 
  geom_ribbon(alpha=.25, show.legend=FALSE) + 
  geom_line() + 
  theme_classic() + 
  theme(legend.position = c(.2, .9)) + 
  labs("x2", y="E(Y|b,X)", linetype="") 
# ggssave("output/f10_7c.png", height=4.5, width=4.5, units="in", dpi=300)


