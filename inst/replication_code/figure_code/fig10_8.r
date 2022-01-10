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


## add in the quantile residuals, 
## standardized deviance residuals, 
## and linear component relating to x2
## into the data for the quadratic true model
hdat1$com_x2 <- predict(h1, type="terms")[,"x2"]
hdat1$stdres <- rstandard(h1, type="deviance")
hdat1$qres1 <- qresid(h1)
hdat1$qres2 <- qresid(h1)
hdat1$qres3 <- qresid(h1)
hdat1$qres4 <- qresid(h1)
hdat1$qres5 <- qresid(h1)
## add a variable indicating which true 
## model relates to the data
hdat1$model <- factor(1, levels=1:3, labels=c("Quadratic", "Log", "Categorical"))

## add in the quantile residuals, 
## standardized deviance residuals, 
## and linear component relating to x2
## into the data for the simple monotone 
## nonlinear true model
hdat2$com_x2 <- predict(h2, type="terms")[,"x2"]
hdat2$stdres <- rstandard(h2, type="deviance")
hdat2$qres1 <- qresid(h2)
hdat2$qres2 <- qresid(h2)
hdat2$qres3 <- qresid(h2)
hdat2$qres4 <- qresid(h2)
hdat2$qres5 <- qresid(h2)
## add a variable indicating which true 
## model relates to the data
hdat2$model <- factor(2, levels=1:3, labels=c("Quadratic", "Log", "Categorical"))

## add in the quantile residuals, 
## standardized deviance residuals, 
## and linear component relating to x2
## into the data for the nominal true model
hdat3$com_x2 <- predict(h3, type="terms")[,"x2"]
hdat3$stdres <- rstandard(h3, type="deviance")
hdat3$qres1 <- qresid(h3)
hdat3$qres2 <- qresid(h3)
hdat3$qres3 <- qresid(h3)
hdat3$qres4 <- qresid(h3)
hdat3$qres5 <- qresid(h3)
## add a variable indicating which true 
## model relates to the data
hdat3$model <- factor(3, levels=1:3, labels=c("Quadratic", "Log", "Categorical"))

## put all data together
h_all <- bind_rows(hdat1, hdat2, hdat3)

## keep relevant variables and average across
## quantile residuals
h_all <- h_all %>% dplyr::select(y, x2, com_x2, stdres, contains("qres"), model) %>%
  rowwise %>% 
  mutate(qr_mean = mean(c(qres1, qres2, qres3, qres4, qres5))) %>% 
  ungroup  %>%
  dplyr::select(-contains("qres"))

## A. Quadratic (standardized deviance residuals)
ggplot(filter(h_all, model=="Quadratic"), aes(x=x2, y=com_x2 + stdres)) + 
  geom_point(shape=1, col="gray50") + 
  geom_smooth(aes(linetype = "Linear"), method="lm", se=FALSE, col="black") + 
  geom_smooth(aes(linetype = "GAM"), method="gam",se=FALSE, col="black") + 
  theme_classic() + 
  theme(legend.position = "top") + 
  labs(x="x2", y="Component + Std. Deviance Residual", linetype = "")
ggsave("output/f10_8a.png", height=4.5, width=4.5, units="in", dpi=300)

## B. Quadratic (randomized quantile residuals)
ggplot(filter(h_all, model=="Quadratic"), aes(x=x2, y=com_x2 + qr_mean)) + 
  geom_point(shape=1, col="gray50") + 
  geom_smooth(aes(linetype = "Linear"), method="lm", se=FALSE, col="black") + 
  geom_smooth(aes(linetype = "GAM"), method="gam",se=FALSE, col="black") + 
  theme_classic() + 
  theme(legend.position = "top") + 
  labs(x="x2", y="Component + Quantile Residual", linetype = "")
ggsave("output/f10_8b.png", height=4.5, width=4.5, units="in", dpi=300)

## C. Simple, monotone (standardized deviance residuals)
ggplot(filter(h_all, model=="Log"), aes(x=x2, y=com_x2 + stdres)) + 
  geom_point(shape=1, col="gray50") + 
  geom_smooth(aes(linetype = "Linear"), method="lm", se=FALSE, col="black") + 
  geom_smooth(aes(linetype = "GAM"), method="gam",se=FALSE, col="black") + 
  theme_classic() + 
  theme(legend.position = "top") + 
  labs(x="x2", y="Component + Std. Deviance Residual", linetype = "")
ggsave("output/f10_8c.png", height=4.5, width=4.5, units="in", dpi=300)

## D. Simple, monotone (randomized quantile residuals)
ggplot(filter(h_all, model=="Log"), aes(x=x2, y=com_x2 + qr_mean)) + 
  geom_point(shape=1, col="gray50") + 
  geom_smooth(aes(linetype = "Linear"), method="lm", se=FALSE, col="black") + 
  geom_smooth(aes(linetype = "GAM"), method="gam",se=FALSE, col="black") + 
  theme_classic() + 
  theme(legend.position = "top") + 
  labs(x="x2", y="Component + Quantile Residual", linetype = "")
ggsave("output/f10_8d.png", height=4.5, width=4.5, units="in", dpi=300)

## E. Categorical (standardized deviance residuals)
ggplot(filter(h_all, model=="Categorical"), aes(x=x2, y=com_x2 + stdres)) + 
  geom_point(shape=1, col="gray50", position=position_jitter(width=.15)) + 
  geom_smooth(aes(linetype = "Linear"), method="lm", se=FALSE, col="black") + 
  geom_smooth(aes(linetype = "GAM"), method="gam", formula =y ~ s(x, k=5), 
              se=FALSE, col="black") + 
  theme_classic() + 
  theme(legend.position = "top") + 
  labs(x="x2", y="Component + Std. Deviance Residual", linetype = "")
ggsave("output/f10_8e.png", height=4.5, width=4.5, units="in", dpi=300)

## F. Categorical (randomized quantile residuals)
ggplot(filter(h_all, model=="Categorical"), aes(x=x2, y=com_x2 + qr_mean)) + 
  geom_point(shape=1, col="gray50", position=position_jitter(width=.15)) + 
  geom_smooth(aes(linetype = "Linear"), method="lm", se=FALSE, col="black") + 
  geom_smooth(aes(linetype = "GAM"), method="gam", formula =y ~ s(x, k=5), 
              se=FALSE, col="black") + 
  theme_classic() + 
  theme(legend.position = "top") + 
  labs(x="x2", y="Component + Quantile Residual", linetype = "")
ggsave("output/f10_8f.png", height=4.5, width=4.5, units="in", dpi=300)

