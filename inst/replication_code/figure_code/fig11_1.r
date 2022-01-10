## load packages
library(tidyverse)

## make data that will be used to make
## the plot.  
x1 <- runif(200, -3, 3)
ystar <- 0 + 2*x1
p <- plogis(ystar)
y <- rbinom(200, 1, p)
plot(x1, p)
m <- glm(y ~ x1, family=binomial)

s <- seq(-3, 3, length=100)
b <- coef(m)
pp <- plogis(b[1] + b[2]*s)

pdf <- data.frame(
  x=s, 
  eta = b[1] + b[2]*s,
  y=pp
)
## make the marginal effect data
mfx_df <- tibble(
  x=c(-2.5, 0, 1.5), 
  eta = c(b[1] + b[2]*x),
  y=plogis(eta), 
  mfx = dlogis(eta)*b[2], 
  int = y-mfx*x)

## make plot
ggplot() + 
  geom_line(data=pdf, aes(x=x, y=pp), col="black") + 
  geom_abline(data=mfx_df, aes(intercept = int, slope=mfx, linetype=as.factor(x)), 
              col="gray50") + 
  theme_classic() + 
  theme(legend.position=c(.8, .5)) + 
  scale_x_continuous(breaks=mfx_df$x) + 
  labs(x=expression(x[1]), y="E(y|b,x1)", linetype=expression(x[1]), parse=TRUE)
ggsave("output/f11_1.png", height=4.5, width=4.5, units="in", dpi=300)
