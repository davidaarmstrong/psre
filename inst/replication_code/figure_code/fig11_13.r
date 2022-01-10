## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(factorplot)
## load data from psre package
data(repress)

## replace pr as NA if pr < 0
repress$pr <- ifelse(repress$pr < 0, NA, repress$pr)
## rescale pr to (0,1)
repress$pr <- repress$pr/100

## create transformed variables and turn binary 
## variables into factors
repress <- repress %>% mutate(log_gdp = log(rgdpe), 
                              logpop = log(pop),
                              pts_fac = as.factor(pts_s), 
                              cwar = as.factor(cwar), 
                              iwar = as.factor(iwar)) %>% 
  dplyr::select(pr, cwar, iwar, rgdpe, pop, log_gdp, logpop, pts_s, pts_fac) %>% 
  na.omit()

## make piecewise linear basis functions
BL <- function(x,c=.34)ifelse(x < c, c-x, 0)
BR <- function(x,c=.34)ifelse(x > c, x-c, 0)

## estimate multinomial logit model
mrm <-nnet::multinom(pts_fac ~ BL(pr, .34) + BR(pr, .34) + cwar + iwar +  
                       log(rgdpe) + log(pop), data=repress)


## get coefficients and variance-covariance matrix from 
## multinomial logit model
b <- coef(mrm)
V <- vcov(mrm)
## turn coefficients into vector from matrix
b <- c(t(b))

## draw 1000 values of coefficients from the appropriate
## random multivariate normal distribution 
B <- MASS::mvrnorm(1000, b, V)
## generate labels appropriate for plotting
cbn <- str_split(colnames(B), pattern = ":", simplify = TRUE)
cbn <- as.data.frame(cbn)
cbn$obs <- 1:nrow(cbn)
## reorganize labels for printing
cbn <- cbn %>% arrange(V2, V1)
## reorganize B values according to the 
## reorganization of the labels
B <- B[,cbn$obs]

## generate the factorplot data
fp <- factorplot:::factorplot.sims(B)

## organize the factorplot data both p-values
## and differences
fpd <- data.frame(
  pval = c(fp$pval), 
  sign = sign(c(fp$b.diff)), 
  rowvar = rep(1:nrow(fp$pval), ncol(fp$pval)), 
  colvar = rep(2:(ncol(fp$pval)+1), each = nrow(fp$pval))
)

## map labels onto the factorplot data
fpd <- na.omit(fpd)
fpd$rvn <- factor(fpd$rowvar, levels=1:28, labels=colnames(B))
fpd$cvn <- factor(fpd$colvar, levels=1:28, labels=colnames(B))

## identify which values are significant
fpd <- fpd %>% 
  mutate(sign_sig = case_when(
    pval < .05 & sign == 1 ~ 1,
    pval >= .05 ~ 3, 
    pval < .05  & sign == -1 ~ 2
  ), 
  sign_sig = factor(sign_sig, levels=1:3, labels=c("+", "-", "Insignificant")))

## make plot
ggplot(fpd) +
  geom_tile(aes(x=colvar, y=rowvar, 
                fill=sign_sig), col="white") + 
  scale_alpha_manual(values=c(1,.2)) + 
  scale_fill_manual(values=c("gray25", "gray50", "gray80")) + 
  geom_hline(yintercept = 4.5 + c(0,4,8,12,16,20), linetype=3, col="gray50") +
  geom_vline(xintercept = 4.5 + c(0,4,8,12,16,20), linetype=3, col="gray50") +
  theme_classic() + 
  scale_x_continuous(breaks=2:28, labels=c(3:5, rep(2:5, 6))) +
  scale_y_continuous(breaks=1:27, labels=c(rep(2:5, 6), 2:4)) +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8), 
        legend.position = "top", 
        plot.margin = margin(0,0,.05,.375, unit="npc")) + 
  coord_cartesian(clip='off') + 
  labs(x="", y="", fill="") + 
  annotation_custom(grob = grid::linesGrob(), 
                    xmin = -1.65, 
                    xmax = -1.65, 
                    ymin = .5, 
                    ymax = 4.25) + 
  annotation_custom(grob = grid::linesGrob(), 
                    xmin = -1.65, 
                    xmax = -1.65, 
                    ymin = 4.75, 
                    ymax = 8.25) + 
  annotation_custom(grob = grid::linesGrob(), 
                    xmin = -1.65, 
                    xmax = -1.65, 
                    ymin = 8.75, 
                    ymax = 12.25)+
  annotation_custom(grob = grid::linesGrob(), 
                    xmin = -1.65, 
                    xmax = -1.65, 
                    ymin = 12.75, 
                    ymax = 16.25)  +
  annotation_custom(grob = grid::linesGrob(), 
                    xmin = -1.65, 
                    xmax = -1.65, 
                    ymin = 16.75, 
                    ymax = 20.25) +
  annotation_custom(grob = grid::linesGrob(), 
                    xmin = -1.65, 
                    xmax = -1.65, 
                    ymin = 20.75, 
                    ymax = 24.25) +
  annotation_custom(grob = grid::linesGrob(), 
                    xmin = -1.65, 
                    xmax = -1.65, 
                    ymin = 24.75, 
                    ymax = 27.25) + 
  annotation_custom(grob = grid::textGrob(label = "(A) Intercept", 
                                          gp = grid::gpar(cex = 1)), 
                    xmin = -5.5-2.5, 
                    xmax = -5.5-2.5, 
                    ymin = 2.375, 
                    ymax= 2.375) +
  annotation_custom(grob = grid::textGrob(label = "(B) BL(Political Rights)", 
                                          gp = grid::gpar(cex = 1)), 
                    xmin = -9.5-2.5, 
                    xmax = -9.5-2.5, 
                    ymin = 6.5, 
                    ymax= 6.5) +
  annotation_custom(grob = grid::textGrob(label = "(C) BR(Political Rights)", 
                                          gp = grid::gpar(cex = 1)), 
                    xmin = -9.5-2.5, 
                    xmax = -9.5-2.5, 
                    ymin = 10.5, 
                    ymax= 10.5) +
  annotation_custom(grob = grid::textGrob(label = "(D) Civil War", 
                                          gp = grid::gpar(cex = 1)), 
                    xmin = -5.5-2.5, 
                    xmax = -5.5-2.5, 
                    ymin = 14.5, 
                    ymax= 14.5) +
  annotation_custom(grob = grid::textGrob(label = "(E) Interstate War", 
                                          gp = grid::gpar(cex = 1)), 
                    xmin = -7.5-2.5, 
                    xmax = -7.5-2.5, 
                    ymin = 18.5, 
                    ymax= 18.5) +
  annotation_custom(grob = grid::textGrob(label = "(F) Population (log)", 
                                          gp = grid::gpar(cex = 1)), 
                    xmin = -8-2.5, 
                    xmax = -8-2.5, 
                    ymin = 22.5, 
                    ymax= 22.5) +
  annotation_custom(grob = grid::textGrob(label = "(G) GDP (log)", 
                                          gp = grid::gpar(cex = 1)), 
                    xmin = -5.75-2.5, 
                    xmax = -5.75-2.5, 
                    ymin = 26, 
                    ymax= 26) + 
  annotation_custom(grob = grid::linesGrob(), 
                    ymin = -2.5, 
                    ymax = -2.5, 
                    xmin = 1.5, 
                    xmax = 4.25) +
  annotation_custom(grob = grid::linesGrob(), 
                    ymin = -2.5, 
                    ymax = -2.5, 
                    xmin = 4.75, 
                    xmax = 8.25) +
  annotation_custom(grob = grid::linesGrob(), 
                    ymin = -2.5, 
                    ymax = -2.5, 
                    xmin = 8.75, 
                    xmax = 12.25) +
  annotation_custom(grob = grid::linesGrob(), 
                    ymin = -2.5, 
                    ymax = -2.5, 
                    xmin = 12.75, 
                    xmax = 16.25) +
  annotation_custom(grob = grid::linesGrob(), 
                    ymin = -2.5, 
                    ymax = -2.5, 
                    xmin = 16.5, 
                    xmax = 20.25) +
  annotation_custom(grob = grid::linesGrob(), 
                    ymin = -2.5, 
                    ymax = -2.5, 
                    xmin = 20.75, 
                    xmax = 24.25) +
  annotation_custom(grob = grid::linesGrob(), 
                    ymin = -2.5, 
                    ymax = -2.5, 
                    xmin = 24.75, 
                    xmax = 28.25) +
  annotation_custom(grob = grid::textGrob(label = "(A)", 
                                          gp = grid::gpar(cex = .8)), 
                    xmin = 3, 
                    xmax = 3, 
                    ymin = -3.55, 
                    ymax= -3.55)+
  annotation_custom(grob = grid::textGrob(label = "(B)", 
                                          gp = grid::gpar(cex = .8)), 
                    xmin = 6.5, 
                    xmax = 6.5, 
                    ymin = -3.55, 
                    ymax= -3.55) +
  annotation_custom(grob = grid::textGrob(label = "(C)", 
                                          gp = grid::gpar(cex = .8)), 
                    xmin = 10.5, 
                    xmax = 10.5, 
                    ymin = -3.55, 
                    ymax= -3.55) +
  annotation_custom(grob = grid::textGrob(label = "(D)", 
                                          gp = grid::gpar(cex = .8)), 
                    xmin = 14.5, 
                    xmax = 14.5, 
                    ymin = -3.55, 
                    ymax= -3.55) +
  annotation_custom(grob = grid::textGrob(label = "(E)", 
                                          gp = grid::gpar(cex = .8)), 
                    xmin = 18.5, 
                    xmax = 18.5, 
                    ymin = -3.55, 
                    ymax= -3.55) +
  annotation_custom(grob = grid::textGrob(label = "(F)", 
                                          gp = grid::gpar(cex = .8)), 
                    xmin = 22.5, 
                    xmax = 22.5, 
                    ymin = -3.55, 
                    ymax= -3.55) +
  annotation_custom(grob = grid::textGrob(label = "(G)", 
                                          gp = grid::gpar(cex = .8)), 
                    xmin = 26.5, 
                    xmax = 26.5, 
                    ymin = -3.55, 
                    ymax= -3.55)  
ggsave("output/f11_13.png", height=4.5, width=4.5, units="in", dpi=300)
