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
data(india)

## Manage india election data
india <- india %>% 
  ## make urban a binary variable
  mutate(urban = case_when(
    urbrural %in% c(1,2) ~ 1, 
    urbrural %in% 3:5 ~ 0, 
    TRUE ~ NA_real_), 
    ## make urban and sbc factors
    urban = as.factor(urban), 
    sbc = as.factor(sbc), 
    ## make bjp a dummy variable indicating
    ## bjp vote
    bjp = case_when(
      in_prty == 2 ~ 1, 
      in_prty %in% c(1,3,4,5) ~ 0, 
      TRUE ~ NA_real_), 
    ## recode ethnicity into broader categories
    eth = case_when(
      in_ethn1 %in% 1:4 ~ "Hindu", 
      in_ethn1 %in% 5:7 ~ "Muslim", 
      in_ethn1 %in% 8:12 ~ "Other", 
      TRUE ~ NA_character_), 
    eth = as.factor(eth), 
    ## make topbot into a three-category variable
    tb3 = case_when(
      topbot %in% 1:3 ~ "Low", 
      topbot %in% 4:7 ~  "Middle", 
      topbot %in% 8:10 ~ "High", 
      TRUE ~ NA_character_), 
    tb3 = factor(tb3, levels=c("Low", "Middle", "High")))

## estimate logit model of bjp vote
mod1 <- glm(bjp ~  eth + sbc + educyrs + tb3 + urban + 
              anti_immigration, 
            data=india, family=binomial)

## estimate logit model with interaction 
## between education and ethnicity
mod1i <- glm(bjp ~  eth*educyrs + sbc + tb3 + urban + 
               anti_immigration, 
             data=india, family=binomial)

## get all variables in mod1 from the 
## india data and listwise delete. 
d <- get_all_vars(mod1, india) %>% na.omit()

## calculate the probabilities of bjp 
## vote for all combinations of education 
## and ethnicity for both the interaction 
## and additive models
pca <- probci(mod1, india, changeX = c("educyrs", "eth"), numQuantVals=21)
names(pca$plot.data) <- c("educyrs", "eth", "pred_prob", "lower", "upper")
pci <- probci(mod1i, india, changeX = c("educyrs", "eth"), numQuantVals=21)
names(pci$plot.data) <- c("educyrs", "eth", "pred_prob", "lower", "upper")

## A. Model 11b (without product term)
## generate the graph of the probability 
## of bjp vote for different values of education
## by ethnicity for the additive model. 
ie_add <- ggplot(pca$plot.data, aes(x=educyrs, y=pred_prob, 
                      ymin=lower, ymax=upper)) + 
  geom_ribbon(aes(fill=eth), alpha=.4) + 
  geom_line(aes(linetype=eth), col="black") + 
  theme_classic() + 
  scale_fill_manual(values=c("gray75", "gray50", "gray15")) + 
  theme(legend.position="bottom") + 
  ylim(0,.85) + 
  labs(x= "Years of Education", y="Predicted Pr(Vote BJP)", 
       linetype="", fill="")

## B. Model 11c (with product term)
## generate the graph of the probability 
## of bjp vote for different values of education
## by ethnicity for the interaction model. 
ie_int <- ggplot(pci$plot.data, aes(x=educyrs, y=pred_prob, 
                                    ymin=lower, ymax=upper)) + 
  geom_ribbon(aes(fill=eth), alpha=.4) + 
  geom_line(aes(linetype=eth), col="black") + 
  theme_classic() + 
  scale_fill_manual(values=c("gray75", "gray50", "gray15")) + 
  theme(legend.position="bottom") + 
  ylim(0,.85) + 
  labs(x= "Years of Education", y="Predicted Pr(Vote BJP)", 
       linetype="", fill="")

## calculate the range of education
ft <- range(d$educyrs)

## calculate the densities of education for each different ethnicity
dens1 <- with(filter(d, eth == "Hindu"), density(educyrs, n=100, from=0, to=20, bw=1.5))
dens1$y <- (dens1$y/max(dens1$y))
dens2 <- with(filter(d, eth == "Muslim"), density(educyrs, n=100, from=0, to=20, bw=1.5))
dens2$y <- (dens2$y/max(dens2$y))
dens3 <- with(filter(d, eth == "Other"), density(educyrs, n=100, from=0, to=20, bw=1.5))
dens3$y <- (dens3$y/max(dens3$y))

## put the densities in a data frame
dens.df <- data.frame(
  x = c(dens1$x, dens2$x, dens3$x), 
  y = c(dens1$y, dens2$y, dens3$y), 
  eth = factor(rep(1:3, each=100), levels=1:3, labels=c("Hindu", "Muslim", "Other"))
)

## Make the marginal density plot
marg_dens <- 
  ggplot(dens.df, aes(x=x, y=y, linetype=eth, fill=eth)) + 
  geom_area(position="identity", alpha=.4, show.legend=FALSE) + 
  geom_line(show.legend=FALSE) + 
  scale_fill_manual(values=c("gray75", "gray50", "gray15")) + 
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(colour="transparent"),
        axis.text.y= element_text(colour="transparent"),
        axis.ticks.y = element_line(colour="transparent")) 


png("output/f11_5a.png", height=5.5, width=4.5, units="in", res=300)  
grid.arrange(marg_dens, ie_add, ncol=1, heights=c(2,8))
dev.off()

png("output/f11_5b.png", height=5.5, width=4.5, units="in", res=300)  
grid.arrange(marg_dens, ie_int, ncol=1, heights=c(2,8))
dev.off()


