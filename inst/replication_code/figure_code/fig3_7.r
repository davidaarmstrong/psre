## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)

## load data from psre package
data(wvs)


## A. All observations

## create data for QQ-plot
qqdf1 <- qqPoints(wvs$gdp_cap)
a <- attr(qqdf1, "ab")[1]
b <- attr(qqdf1, "ab")[2]
l1 <- min(qqdf1$theo) * b + a
u1 <- max(qqdf1$theo) * b + a

## mark points that are outside of the confidence bounds
## with the variable 'outside'
qqdf1 <- qqdf1 %>% mutate(outside = factor(ifelse(x < lwr | x > upr, 2, 1)))

## make the plot
ggplot(qqdf1, aes(x=theo, y=x)) + 
    geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15) + 
    geom_segment(aes(x=min(theo), xend=max(theo), y = l1, yend=u1)) + 
    geom_point(shape=1, show.legend = FALSE) + 
    theme_classic() + 
    theme(aspect.ratio=1) + 
    labs(x="Theoretical Quantiles", 
         y="Observed Quantiles")
ggsave("output/f3_7a.png", height=4.5, width=4.5, units="in", dpi=300)


## B. New Democracies
qqdf2 <- qqPoints(wvs$gdp_cap[which(wvs$democrat == 1)])
a <- attr(qqdf2, "ab")[1]
b <- attr(qqdf2, "ab")[2]
l2 <- min(qqdf2$theo) * b + a
u2 <- max(qqdf2$theo) * b + a
qqdf2 <- qqdf2 %>% mutate(outside = factor(ifelse(x < lwr | x > upr, 2, 1)))
{ggplot(qqdf2, aes(x=theo, y=x)) + 
    geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15) + 
    geom_segment(aes(x=min(theo), xend=max(theo), y = l2, yend=u2)) + 
    geom_point(shape=1, show.legend = FALSE) + 
    theme_classic() + 
    theme(aspect.ratio=1) + 
    labs(x="Theoretical Quantiles", 
         y="Observed Quantiles")
}
ggsave("output/f3_7b.png", height=4.5, width=4.5, units="in", dpi=300)

## C. Established Democracies

qqdf3 <- qqPoints(wvs$gdp_cap[which(wvs$democrat == 2)])
qqdf3$lab <- NA
qqdf3$lab[1:3] <- c("India 1990", "Mali 2007", "India 1995")

library(ggrepel)  
a <- attr(qqdf3, "ab")[1]
b <- attr(qqdf3, "ab")[2]
l3 <- min(qqdf3$theo) * b + a
u3 <- max(qqdf3$theo) * b + a
qqdf3 <- qqdf3 %>% mutate(outside = factor(ifelse(x < lwr | x > upr, 2, 1)))
{ggplot(qqdf3, aes(x=theo, y=x)) + 
    geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15) + 
    geom_segment(aes(x=min(theo), xend=max(theo), y = l3, yend=u3)) + 
    geom_point(shape=1, show.legend = FALSE) + 
    geom_label_repel(aes(label=lab), nudge_y=c(-5000, 5000, -10000)) + 
    theme_classic() + 
    theme(aspect.ratio=1) + 
    labs(x="Theoretical Quantiles", 
         y="Observed Quantiles")}
ggsave("output/f3_7c.png", height=4.5, width=4.5, units="in", dpi=300)

