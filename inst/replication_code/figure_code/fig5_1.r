## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)

## load data from psre package
data(ces)

## add row number (observation) variable to ces
ces$rownum <- 1:nrow(ces)

## make gender a factor increase variance in moral and 
## decrease variance in leader_con 
ces <- ces %>% 
  mutate(gender = factor(gender, levels=c(1,5), labels=c("male", "female")), 
         moral = (moral + 1)*50, 
         leader_con = leader_con/20)

## identify row numbers to be extracted from data
rn <- c(52, 133, 175, 193, 285, 320, 336, 428, 543, 560, 650, 695, 709, 804, 913,
        974,1065,1172,1186,1220,1235,1237,1307,1365,1420,1484,1517,1523,1554,1609,
        1684,1690,1768,1918,1970,1986,2053,2067,2137,2229,2239,2371,2403,2432,2515,
        2591,2594,2701,2759,2799)

## extract only identified rows from data
ces_samp <- ces %>% 
  filter(rownum %in% rn)

## estimate model
mod <- lm(moral ~ gender + leader_con, data=ces_samp)

## save model coefficients as 'b'
b <- coef(mod)
## intercept for men is global intercept
b0m <- b[1]
## intercept for women is global intercept 
## plus coefficient on gender dummy regressor
b0f <- b[1] + b[2]
## 'bl' is the slope of leader_con for all obs
bl = b[3]

## A. With points
f5_1a <- ggplot(ces_samp, aes(x=leader_con, y=moral, colour=gender)) + 
  geom_point() + 
  geom_abline(slope=bl, intercept=b0m, lty=1, color="gray50") + 
  geom_abline(slope=bl, intercept=b0f, lty=2, color="black") + 
  scale_colour_manual(values=c("gray50", "black")) + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="Conservative Leader Feeling Thermometer", 
       y="Moral Traditionalism", 
       colour="")
f5_1a
# ggssave("output/f5_1a.png", height=5, width=4.5, units="in", dpi=300)



## B. Without Points (with explanatory lines)
## make all combinations of leader_con={1,2,3,4,5} and 
## gender = {male, female}
fd <- expand.grid(leader_con=c(1,2,3,4,5), 
                  gender=factor(c(1,5), levels=c(1,5), labels=c("male", "female")))
## add variable fit, which are predicted values based on 
## the combinations above. 
fd <- cbind(fd, fit=predict(mod, newdata=fd))

## build the previous plot to retrieve the 
## axis ranges. 
f5_1ab <- ggplot_build(f5_1a)
## get the y-axis range from the built ggplot
rgy <- f5_1ab$layout$panel_params[[1]]$y.range

## make plot
ggplot(ces_samp, aes(x=leader_con, y=moral)) + 
  geom_point(col="transparent") + 
  geom_abline(aes(linetype="male", colour="male", 
                  slope=bl, intercept=b0m)) + 
  geom_abline(aes(linetype="female", colour="female", 
                  slope=bl, intercept=b0f)) + 
  geom_segment(aes(x=1, y=fd$fit[1], xend=1, yend=fd$fit[6]), 
               arrow=arrow(length=unit(.25, "cm"), ends="both")) + 
  geom_segment(aes(x=4, y=fd$fit[4], xend=4, yend=fd$fit[9]), 
               arrow=arrow(length=unit(.25, "cm"), ends="both")) + 
  geom_segment(aes(x=2, y=fd$fit[2], xend=3, yend=fd$fit[2]), 
               arrow=arrow(length=unit(.25, "cm"), ends="last")) + 
  geom_segment(aes(x=2, y=fd$fit[7], xend=3, yend=fd$fit[7]), 
               arrow=arrow(length=unit(.25, "cm"), ends="last")) + 
  geom_segment(aes(x=3, y=fd$fit[2], xend=3, yend=fd$fit[3]), 
               arrow=arrow(length=unit(.25, "cm"), ends="last")) + 
  geom_segment(aes(x=3, y=fd$fit[7], xend=3, yend=fd$fit[8]), 
               arrow=arrow(length=unit(.25, "cm"), ends="last")) + 
  geom_text(x=1.2, y=mean(fd$fit[c(1,6)]), label="b[2]", parse=TRUE) +
  geom_text(x=4.2, y=mean(fd$fit[c(4,9)]), label="b[2]", parse=TRUE) +
  geom_text(x=3.2, y=mean(fd$fit[c(2,3)]), label="b[1]", parse=TRUE) +
  geom_text(x=3.2, y=mean(fd$fit[c(7,8)]), label="b[1]", parse=TRUE) +
  geom_text(x=2.5, y=fd$fit[2]-1.75, label="1") +
  geom_text(x=2.5, y=fd$fit[7]-1.75, label="1") +
  scale_colour_manual(values=c("black", "gray50"))+ 
  scale_linetype_manual(values=c(2,1)) + 
  scale_y_continuous(breaks=c(20, b[1], 40, 60), 
                     labels=c("20", "b0", "40", "60")) + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="Conservative Leader Feeling Thermometer", 
       y="Moral Traditionalism", 
       colour="", linetype="") + 
  coord_cartesian(xlim=c(0,5), ylim=rgy, expand=FALSE)
# ggssave("output/f5_1b.png", height=5, width=4.5, units="in", dpi=300)

## C. Two Examples
## get x- and y-axis ranges from the plot in panel A
xrg <- ggplot_build(f5_1a)$layout$panel_params[[1]]$x.range
yrg <- ggplot_build(f5_1a)$layout$panel_params[[1]]$y.range

## get predictions for observations 974 and 2403
fit2 <-   ces_samp %>% 
  filter(rownum %in% c(974, 2403)) 
## create ey which will be the location of the label for
## the residual. 
fit2 <- cbind(fit2, fit=predict(mod, newdata=fit2)) %>% 
  rowwise %>% 
  mutate(ey = mean(c(fit, moral)))


## make plot
ggplot(fit2, aes(x=leader_con, y=moral, colour=gender)) + 
  geom_point() + 
  geom_point(aes(y=fit)) + 
  geom_abline(slope=bl, intercept=b0m, lty=1, color="gray50") + 
  geom_abline(slope=bl, intercept=b0f, lty=2, color="black") + 
  scale_colour_manual(values=c("gray50", "black")) + 
  geom_segment(aes(xend=leader_con, yend=fit)) + 
  geom_text(aes(x=leader_con + .2, y=ey, label=c("e[1]", "e[2]")), parse=TRUE, col="black") + 
  geom_text(aes(x=leader_con, y=moral, label=c("y[1]", "y[2]")), parse=TRUE, col="black", 
            nudge_y=c(3,-3)) + 
  geom_text(aes(x=leader_con, y=fit, label=c("hat(y)[1]", "hat(y)[2]")), parse=TRUE, col="black", 
            nudge_y=c(-3,3)) + 
  coord_cartesian(xlim=xrg, ylim=yrg, expand=FALSE) + 
  theme_classic() + 
  theme(legend.position="top") + 
  labs(x="Conservative Leader Feeling Thermometer", 
       y="Moral Traditionalism", 
       colour="")
# ggssave("output/f5_1c.png", height=5, width=4.5, units="in", dpi=300)

