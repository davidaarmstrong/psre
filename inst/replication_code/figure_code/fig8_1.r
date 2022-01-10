## bulging Rule plot
library(ggplot2)
## create sequence of x-values to use for the bluging plot
x <- 2:40
y1 <- log(x)
y2 <- rev(y1)

## make data we will use to plot
df <-  data.frame(x = c(x, x, 50:88, 50:88), 
                  y=c(y1, -y1, y2, -y2), 
                  group = rep(1:4, each=39))

## identify the limits of the axes
xl <- range(df$x) + c(-.15,.15)*diff(range(df$x))
yl <- range(df$y) + c(-.15,.15)*diff(range(df$y))

## make the plot
ggplot() + 
  geom_line(data=df, aes(x=x, y=y, group=group)) +
  geom_vline(xintercept=45) + 
  geom_hline(yintercept=0) + 
  annotate("text", x = xl[1], y = yl[1] + .05*diff(yl), parse = TRUE,
           label = "plain(x)~plain(toward)~list(log(x), -1/x, etc...)~OR", hjust=0) + 
  annotate("text", x = xl[1], y = yl[1] + .0*diff(yl), parse = TRUE,
           label = "plain(y)~plain(toward)~list(log(y), -1/y, etc...)", hjust=0) + 
  annotate("text", x = xl[1], y = yl[2], parse = TRUE,
           label = "plain(x)~plain(toward)~list(log(x), -1/x, etc...)~OR", hjust=0) + 
  annotate("text", x = xl[1], y = yl[2] - .05*diff(yl), parse = TRUE,
           label = "plain(y)~plain(toward)~list(y^2, y^3, etc...)", hjust=0) + 
  annotate("text", x = xl[2], y = yl[2], parse = TRUE,
           label = "plain(x)~plain(toward)~list(x^2, x^3, etc...)~OR", hjust=1) + 
  annotate("text", x = xl[2], y = yl[2] - .05*diff(yl), parse = TRUE,
           label = "plain(y)~plain(toward)~list(y^2, y^3, etc...)", hjust=1) + 
  annotate("text", x = xl[2], y = yl[1] + .05*diff(yl), parse = TRUE,
           label = "plain(x)~plain(toward)~list(x^2, x^3, etc...)~OR", hjust=1) + 
  annotate("text", x = xl[2], y = yl[1] + .0*diff(yl), parse = TRUE,
           label = "plain(y)~plain(toward)~list(log(y), -1/y, etc...)", hjust=1) + 
  theme_void() + 
  coord_cartesian(xlim=xl, 
                  ylim=yl)
ggsave("output/f8_1.png", height=5.5, width=5.5, units="in", dpi=300)
             