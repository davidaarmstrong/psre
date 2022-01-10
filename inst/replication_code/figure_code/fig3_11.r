## Note: There is a discrepancy between this figure and the one that 
## is in the book.  

## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages

library(tidyverse)
library(psre)
library(sn)
library(car)
library(cellWise)

## load data from psre package
data(wvs)

## keep the gdp_capita variable and rename it 'raw' - store it in the 
## data frame 'tmp1'
tmp1 <- wvs %>% select(gdp_cap) %>% setNames("raw")
## find the appropriate lambda and gamma parameters for the BCn transformation
p1n <- powerTransform(tmp1$raw ~ 1, family="bcnPower")
## add the BCn transformed values to the dataset as the variable 'bcn'
tmp1 <- tmp1 %>% mutate(bcn = bcnPower(raw, lambda = p1n$roundlam, gamma=p1n$gamma)) 
## add the Raymaekers and Rousseuwtransformed values to the dataset as the variable 'rr'
tmp1 <- tmp1 %>% mutate(rr = unname(c(transfo(raw, type="YJ", robust=TRUE)$Xt)))
## add the Velez et al transformed values to the dataset as the variable 'v'
## then, pivot the data longer and add a variable called 'var' that identifies
## the input variable to the transformation. 
tmp1 <- tmp1 %>% mutate(v = unname(yjPower(raw, lambda = transNorm(raw, 
                                                                 family="yj",
                                                                 lams =c(-2,2))))) %>% 
  pivot_longer(everything(), names_to="trans", values_to="vals") %>% 
  mutate(trans = factor(trans, levels=c("raw", "bcn", "v", "rr"), 
                        labels=c("Raw", "BCn", "Velez", "Raymaekers")), 
         var = "GDP per capita")

## Proceed just as above, but for the population variable. 
tmp2 <- wvs %>% select(pop) %>% setNames("raw")
p2n <- powerTransform(tmp2$raw ~ 1, family="bcnPower")
tmp2 <- tmp2 %>% mutate(bcn = bcnPower(raw, lambda = p2n$roundlam, gamma=p2n$gamma)) 
tmp2 <- tmp2 %>% mutate(rr = unname(c(transfo(raw, type="YJ", robust=TRUE)$Xt)))
tmp2 <- tmp2 %>% mutate(v = unname(yjPower(raw, lambda = transNorm(raw, 
                                                                   family="yj",
                                                                   lams =c(-2,2))))) %>% 
  pivot_longer(everything(), names_to="trans", values_to="vals") %>% 
  mutate(trans = factor(trans, levels=c("raw", "bcn", "v", "rr"), 
                        labels=c("Raw", "BCn", "Velez", "Raymaekers")), 
         var = "Population")

## put both temporary data frames together
tmp_all <- bind_rows(tmp1, tmp2)
## calculate the densities of each transformation-variable pair
tmp_dens <- tmp_all %>% group_by(trans, var) %>% 
  summarise(normBand(vals))

## make the plot
ggplot(tmp_dens, aes(x=eval.points)) + 
  geom_ribbon(aes(ymin = lwr, ymax=upr), alpha=.25, fill="gray50") + 
  geom_ribbon(aes(ymin = lwd_od, ymax = upr_od), col="transparent", alpha=.5) + 
  geom_line(aes(y=obsden), col="black") + 
  facet_wrap(var ~ trans, scales="free", ncol=4) + 
  theme_bw() + 
  theme(panel.grid=element_blank(),
        aspect.ratio=1) + 
  labs(x="Transformed Variable Values", y="Density")
ggsave("output/f3_11.png", height=4, width=7, units="in", dpi=300)
