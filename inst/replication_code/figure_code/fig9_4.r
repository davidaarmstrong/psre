## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(DAMisc)
library(gridExtra)

## load data from psre package
data(wvs)

## reduce civ to 3 categories
wvs1 <- wvs %>% 
  mutate(
    civ2 = case_when(
      civ == 9 ~ "Western", 
      civ == 6 ~ "Latin American", 
      TRUE ~ "Other"),
    civ2 = factor(civ2, levels=c("Western", "Latin American", "Other")), 
    democrat= factor(democrat, levels=1:2, 
                     labels=c("New Democracy", "Established Democracy")))

## estimate interaction model
intmod2 <- lm(resemaval ~ civ2 * pct_secondary, data=wvs1)

## find the effect of comparisons of categorical variable levels
## in the interaction model 
iqq <- intQualQuant(intmod2, c("civ2", "pct_secondary"), type="facs", plot=FALSE)
iqq <- iqq$out
iqq <- iqq %>% separate(contrast, sep="-", into=c("c1", "c2"), remove=FALSE)
base <- iqq %>% group_by(contrast) %>% summarise(across(c(c1, c2), ~ first(.x)))

## calculate the densities of pct_secondary for each value of 
## the categorical variable in the pair for each comparison 
dens_dat <- NULL
for(i in 1:nrow(base)){
  v1 <- wvs1$pct_secondary[which(wvs1$civ2 == trimws(base$c1[i]))]
  v2 <- wvs1$pct_secondary[which(wvs1$civ2 == trimws(base$c2[i]))]
  {if(length(v1) > length(v2)){
    v2 <- c(v2, rep(NA, (length(v1)-length(v2))))
  }else{
    v1 <- c(v1, rep(NA, (length(v2)-length(v1))))
  }}
  tmp <- data.frame(
    contrast = base$contrast[i], 
    var1 = v1, 
    var2 = v2)
  dens_dat <- rbind(dens_dat, tmp)
}

## Make the contrasts
contrs <- iqq %>% 
  group_by(contrast) %>% 
  summarise(n = n()) %>% 
  separate(contrast, se = " - ", into=c("c1", "c2"), remove=FALSE)

## Filter to include only the second contrast
i <- 2
tmp <- iqq %>% filter(contrast == contrs$contrast[i])

## make the main panel
g9_4a1 <- ggplot(tmp, aes(x=x, y=fit, ymin=lower, ymax=upper)) + 
  geom_ribbon(alpha=.25) + 
  geom_line() + 
  geom_hline(yintercept=0, linetype=3) + 
  theme_classic() + 
  labs(x="Proportion with Secondary Education", 
       y="Difference in Predicted Values\n(95% Confidence Envelope)")
## get the y-range for the main panel
p1 <- ggplot_build(g9_4a1)
rgy <- p1$layout$panel_params[[1]]$y.range
maxy <- rgy[1] + .3*diff(rgy)

tmpw <- wvs1 %>% filter(civ2 %in% c(contrs$c1[i], contrs$c2[i])) 
## calculate the 95% highest density region for each group in the comparison
linevals <- tmpw %>% group_by(civ2) %>% 
  summarise(low = coda::HPDinterval(coda::as.mcmc(pct_secondary))[1], 
            high = coda::HPDinterval(coda::as.mcmc(pct_secondary))[2]) %>%
  ungroup %>% 
  ## record the higher of the two lower bounds and the 
  ## lower of the two upper bounds 
  summarise(low = max(low), high = min(high))

## plot the upper panel - along with the overlapping highest density line
g9_4a2 <- wvs1 %>% filter(civ2 %in% c(contrs$c1[i], contrs$c2[i])) %>% 
  ggplot(aes(x=pct_secondary, y=..density../sum(..density..), fill=civ2, 
             linetype=civ2)) + 
  geom_density(position="identity", alpha=.2, show.legend=FALSE) + 
  geom_segment(aes(x=linevals$low, xend=linevals$high, y=0, yend=0), size=1, show.legend=FALSE) + 
  labs(x="", y="", fill="Civilization")  + 
  scale_fill_manual(values=c("gray25", "gray50")) + 
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(colour="transparent"),
        axis.text.y=element_text(colour="transparent"),
        axis.ticks.y = element_line(colour="transparent")) 

png("output/f9_4a.png", height=5.5, width=4.5, units="in", res=300)  
grid.arrange(g9_4a2, g9_4a1, ncol=1, heights=c(2,8))
dev.off()

## proceed as above, but for the third contrast

i <- 3
tmp <- iqq %>% filter(contrast == contrs$contrast[i])

g9_4b1 <- ggplot(tmp, aes(x=x, y=fit, ymin=lower, ymax=upper)) + 
  geom_ribbon(alpha=.25) + 
  geom_line() + 
  geom_hline(yintercept=0, linetype=3) + 
  theme_classic() + 
  labs(x="Proportion with Secondary Education", 
       y="Difference in Predicted Values\n(95% Confidence Envelope)")
p1 <- ggplot_build(g9_4b1)
rgy <- p1$layout$panel_params[[1]]$y.range
maxy <- rgy[1] + .3*diff(rgy)

tmpw <- wvs1 %>% filter(civ2 %in% c(contrs$c1[i], contrs$c2[i])) 
linevals <- tmpw %>% group_by(civ2) %>% 
  summarise(low = coda::HPDinterval(coda::as.mcmc(pct_secondary))[1], 
            high = coda::HPDinterval(coda::as.mcmc(pct_secondary))[2]) %>%
  ungroup %>% 
  summarise(low = max(low), high = min(high))

g9_4b2 <- wvs1 %>% filter(civ2 %in% c(contrs$c1[i], contrs$c2[i])) %>% 
  ggplot(aes(x=pct_secondary, y=..density../sum(..density..), fill=civ2, 
             linetype=civ2)) + 
  geom_density(position="identity", alpha=.2, show.legend=FALSE) + 
  geom_segment(aes(x=linevals$low, xend=linevals$high, y=0, yend=0), size=1, show.legend=FALSE) + 
  labs(x="", y="", fill="Civilization")  + 
  scale_fill_manual(values=c("gray25", "gray50")) + 
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(colour="transparent"),
        axis.text.y=element_text(colour="transparent"),
        axis.ticks.y = element_line(colour="transparent")) 

png("output/f9_4b.png", height=5.5, width=4.5, units="in", res=300)  
grid.arrange(g9_4b2, g9_4b1, ncol=1, heights=c(2,8))
dev.off()

## proceed as above, but for the first contrast
i <- 1
tmp <- iqq %>% filter(contrast == contrs$contrast[i])

g9_4c1 <- ggplot(tmp, aes(x=x, y=fit, ymin=lower, ymax=upper)) + 
  geom_ribbon(alpha=.25) + 
  geom_line() + 
  geom_hline(yintercept=0, linetype=3) + 
  theme_classic() + 
  labs(x="Proportion with Secondary Education", 
       y="Difference in Predicted Values\n(95% Confidence Envelope)")
p1 <- ggplot_build(g9_4a1)
rgy <- p1$layout$panel_params[[1]]$y.range
maxy <- rgy[1] + .3*diff(rgy)

tmpw <- wvs1 %>% filter(civ2 %in% c(contrs$c1[i], contrs$c2[i])) 
linevals <- tmpw %>% group_by(civ2) %>% 
  summarise(low = coda::HPDinterval(coda::as.mcmc(pct_secondary))[1], 
            high = coda::HPDinterval(coda::as.mcmc(pct_secondary))[2]) %>%
  ungroup %>% 
  summarise(low = max(low), high = min(high))

g9_4c2 <- wvs1 %>% filter(civ2 %in% c(contrs$c1[i], contrs$c2[i])) %>% 
  ggplot(aes(x=pct_secondary, y=..density../sum(..density..), fill=civ2, 
             linetype=civ2)) + 
  geom_density(position="identity", alpha=.2, show.legend=FALSE) + 
  geom_segment(aes(x=linevals$low, xend=linevals$high, y=0, yend=0), size=1, show.legend=FALSE) + 
  labs(x="", y="", fill="Civilization")  + 
  scale_fill_manual(values=c("gray25", "gray50")) + 
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(colour="transparent"),
        axis.text.y=element_text(colour="transparent"),
        axis.ticks.y = element_line(colour="transparent")) 

png("output/f9_4c.png", height=5.5, width=4.5, units="in", res=300)  
grid.arrange(g9_4c2, g9_4c1, ncol=1, heights=c(2,8))
dev.off()









