## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(psre)
library(tidyverse)

## load data from psre package
data(gss)

## keep data where values of education >= 9
gss %>% dplyr::filter(educ >= 9) %>% 
  ungroup %>% 
  ## create real-income factor that splits real income at its median 
  mutate(realinc_fac = case_when(
    realinc > median(realinc, na.rm=TRUE) ~ 2, 
    realinc <= median(realinc, na.rm=TRUE) ~ 1, 
    TRUE ~ NA_real_), 
    realinc_fac = factor(realinc_fac, levels=1:2, labels=c("Income <= Median", "Income > Median")), 
    ## create sei10 factor that splits it at its median
    sei10_fac = case_when(
      sei10 > median(sei10, na.rm=TRUE) ~ 2, 
      sei10 <= median(sei10, na.rm=TRUE) ~ 1, 
      TRUE ~ NA_real_), 
    sei10_fac = factor(sei10_fac, levels=1:2, labels=c("SEI <= Median", "SEI > Median"))
  )%>%
  ## remove missing values on either of the new factors
  dplyr::filter(!is.na(sei10_fac) & !is.na(realinc_fac)) %>% 
  ## make the plot
  ggplot(aes(x=educ, y=aid_scale)) + 
  geom_point(shape=1, col="gray50", position=position_jitter(width=.5)) + 
  facet_grid(sei10_fac ~ realinc_fac) + 
  geom_smooth(method="lm", col="black", se=FALSE) + 
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        aspect.ratio = 1) + 
  labs(x="Years of Formal Education", 
       y = "Aid Generosity Scale")
ggsave("output/f4_21.png", height=5, width=5, units="in", dpi=300)
