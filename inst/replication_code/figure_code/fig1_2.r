library(ggplot2)
## Panel (a): Bias 
## make data 
bias.dat <- data.frame(
  x1 = seq(-2,1, length=50), 
  x2 = seq(-1,2, length=50) 
)

## generate plot
ggplot(bias.dat) + 
  geom_area(aes(x=x1, y=dnorm(x1, 0, sd=.5), fill="Biased"), alpha=.3) + 
  geom_area(aes(x=x2, y=dnorm(x2, .5, sd=.5), fill="Unbiased"), alpha=.3) + 
  scale_x_continuous(breaks=.5, labels = expression(beta)) + 
  geom_vline(xintercept=.5, linetype=3) + 
  scale_fill_manual(values=c("gray75", "gray50")) + 
  labs(x="b", y="Density", fill="") + 
  theme_classic() + 
  theme(legend.position="top")
## ggssave("output/f1_2a.png", height=4.5, width=4.5, dpi=300)

## Panel (b): Efficiency
## make data
xdat <- data.frame(x=seq(-3,3,length=100))

## generate plot
ggplot(xdat, aes(x=x)) + 
  geom_function(aes(linetype="Inefficient"), 
                fun=~dnorm(.x, 0, 1)) + 
  geom_function(aes(linetype="Efficient"), 
                fun=~dnorm(.x, 0, .5)) + 
  scale_x_continuous(breaks=0, labels = expression(beta)) + 
  theme_classic() + 
  labs(x="b", y="Density", linetype="") + 
  theme(legend.position=c(.85,.85))
## ggssave("output/f1_2b.png", height=4.5, width=4.5, dpi=300)


## Panel (c): Consistency 
## generate plot, using efficiency data
ggplot(xdat, aes(x=x)) + 
  geom_function(aes(linetype="n=100"), fun=~dnorm(.x, sd=1)) + 
  geom_function(aes(linetype="n=100000"), fun=~dnorm(.x, sd=.25)) + 
  geom_segment(aes(x=0, xend=0, y=0, yend=2, linetype="n=infinity")) + 
  scale_linetype_manual(values=c(2,3,1)) + 
  scale_x_continuous(breaks=0, labels = expression(beta)) + 
  theme_classic() + 
  theme(legend.position=c(.85, .85)) + 
  labs(x="b", y="Density", linetype="")
## ggssave("output/f1_2c.png", height=4.5, width=4.5, dpi=300)

