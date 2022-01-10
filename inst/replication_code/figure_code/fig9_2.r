## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(ggeffects)
library(qvcalc)

## load data from psre package
data(wvs)

## estimate the interaction model by making all 
## combinations of the three-category civilization 
## variable and the two-category democratic history 
## variable. 
intmod1a <- wvs %>% 
  mutate(
    civa = case_when(
      civ == 9 ~ "W", 
      civ == 6 ~ "LA", 
      TRUE ~ "O"), 
    dema = case_when(
      democrat == 2 ~ "ED", 
      democrat == 1 ~ "ND", 
      TRUE ~ NA_character_), 
    civ_dem = paste(civa, dema, sep="_"), 
    civ_dem = factor(civ_dem)) %>% 
  lm(resemaval ~ civ_dem, data=.)

## generate predicted values from the model with 
## 84% confidence intervals. 
g <- ggpredict(intmod1a, "civ_dem", level=84)

## make the design matrix from the model.
X <- model.matrix(intmod1a, data=data.frame(civ_dem = g$x, resemaval = 0))

## Calculate the fitted values and variance-covariance
## matrix of those fitted values. 
fit <- c(X %*% coef(intmod1a))
V <- X %*% vcov(intmod1a) %*% t(X)
## calculate quasi-variances for the civ_dem variable
qvf <- qvcalc(intmod1a, "civ_dem")$qvframe
## find the optimal confidence level for visual testing.
o_int <- optCL(intmod1a, "civ_dem", quasi_vars = qvf[,4], add_ref=TRUE)

## make the data for the plot
g <- tibble(
  x = g$x,
  predicted = fit, 
  se = qvf[,3], 
  conf.low = predicted - qt(1-(1-.84)/2, intmod1a$df.residual)*se,
  conf.high = predicted +qt(1-(1-.84)/2, intmod1a$df.residual)*se
)


## break apart the civ and dem values
g <- g %>% separate(x,into=c("civ", "dem"), remove=FALSE) %>% mutate(dem = factor(dem, levels=c("ND", "ED")))
## order by dem and then by civ within dem
g <- g %>% arrange(dem, civ)
## make a new variable called 'newx' which is a factor 
## that has as its levels the category labels of the original factor variable
g <- g %>% mutate(newx = factor(1:6, labels=x))
## Change the levels to something shorter
g$newx <- factor(
  as.character(g$newx), 
  levels=c("O_ND", "LA_ND", "W_ND", "O_ED", "LA_ED", "W_ED"))

## make plot
ggplot(g, aes(y=newx)) + 
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), height=.15) + 
  geom_point(aes(x=predicted)) + 
  labs(y="", x="Coefficient\n(84% Quasi-confidence Interval)") + 
  theme_classic() + 
  coord_cartesian(clip='off') + 
  theme(plot.margin = margin(0,0,0,.075, unit="npc")) + 
  annotation_custom(grob = grid::linesGrob(), 
                    xmin = .23, 
                    xmax = .23, 
                    ymin = 6.4, 
                    ymax = 3.6) + 
  annotation_custom(grob = grid::textGrob(label = "Estalished Democracies", 
                                          gp = grid::gpar(cex = 1), rot = 90), 
                    xmin = .22, 
                    xmax = .22, 
                    ymin = 5, 
                    ymax=5) + 
  annotation_custom(grob = grid::linesGrob(), 
                    xmin = .23, 
                    xmax = .23, 
                    ymin = 3.4, 
                    ymax = .6) + 
  annotation_custom(grob = grid::textGrob(label = "New Democracies", 
                                          gp = grid::gpar(cex = 1), rot = 90), 
                    xmin = .22, 
                    xmax = .22, 
                    ymin = 2, 
                    ymax=2) + 
  scale_y_discrete(breaks=c("O_ND", "W_ND", "LA_ND", "O_ED", "LA_ED", "W_ED"), 
                   labels=c("Other (n=67)", "Western (n=5)", "Latin Amer (n=18)", "Other (n=22)", "Latin Amer (n=10)", "Western (n=40)"))
ggsave("output/f9_2.png", height=4.5, width=4.5, units="in", dpi=300)
