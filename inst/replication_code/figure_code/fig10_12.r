## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
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


## get quantile residuals for each mis-specified model
eq1 <- qresid(h1)
eq2 <- qresid(h2)
eq3 <- qresid(h3)


## put quantile residuals, standardized deviance
## residuals, fitted values and constant information
## scale in same data frame
res_dat <- tibble(
  fit = c(fitted(h1), fitted(h2), fitted(h3)), 
  eq = c(eq1, eq2, eq3), 
  ed = c(rstandard(h1, type="deviance"), rstandard(h2, type="deviance"), 
         rstandard(h3, type="deviance")), 
  cis_fit = sqrt(fit), 
  model = factor(rep(1:3, each=1000), 
                 levels=1:3, 
                 labels=c("Quadratic", "Log", "Categorical"))
)

## A. Quadratic (standardized deviance residuals)
ggplot(filter(res_dat, model == "Quadratic"), 
       aes(x=cis_fit, y=ed)) + 
  geom_point(shape=1, col="gray50") + 
  geom_smooth(method="loess", se=FALSE, col="black") + 
  theme_classic() + 
  labs(x="Constant Information Scale", y="Standardized Deviance Residuals")
# ggssave("output/f10_12a.png", height=4.5, width=4.5, units="in", dpi=300)

## B. Quadratic (randomized quantile residuals)
ggplot(filter(res_dat, model == "Quadratic"), 
       aes(x=cis_fit, y=eq)) + 
  geom_point(shape=1, col="gray50") + 
  geom_smooth(method="loess", se=FALSE, col="black") + 
  theme_classic() + 
  labs(x="Constant Information Scale", y="Quantile Residuals")
# ggssave("output/f10_12b.png", height=4.5, width=4.5, units="in", dpi=300)

## C. Simple, monotone (standardized deviance residuals)
ggplot(filter(res_dat, model == "Log"), 
       aes(x=cis_fit, y=ed)) + 
  geom_point(shape=1, col="gray50") + 
  geom_smooth(method="loess", se=FALSE, col="black") + 
  theme_classic() + 
  labs(x="Constant Information Scale", y="Standardized Deviance Residuals")
# ggssave("output/f10_12c.png", height=4.5, width=4.5, units="in", dpi=300)

## D. Simple, monotone (randomized quantile residuals)
ggplot(filter(res_dat, model == "Log"), 
       aes(x=cis_fit, y=eq)) + 
  geom_point(shape=1, col="gray50") + 
  geom_smooth(method="loess", se=FALSE, col="black") + 
  theme_classic() + 
  labs(x="Constant Information Scale", y="Quantile Residuals")
# ggssave("output/f10_12d.png", height=4.5, width=4.5, units="in", dpi=300)

## E. Categorical (standardized deviance residuals)
ggplot(filter(res_dat, model == "Categorical"), 
       aes(x=cis_fit, y=ed)) + 
  geom_point(shape=1, col="gray50") + 
  geom_smooth(method="loess", se=FALSE, col="black") + 
  theme_classic() + 
  labs(x="Constant Information Scale", y="Standardized Deviance Residuals")
# ggssave("output/f10_12e.png", height=4.5, width=4.5, units="in", dpi=300)

## F. Categorical (randomized quantile residuals)
ggplot(filter(res_dat, model == "Categorical"), 
       aes(x=cis_fit, y=eq)) + 
  geom_point(shape=1, col="gray50") + 
  geom_smooth(method="loess", se=FALSE, col="black") + 
  theme_classic() + 
  labs(x="Constant Information Scale", y="Quantile Residuals")
# ggssave("output/f10_12f.png", height=4.5, width=4.5, units="in", dpi=300)

