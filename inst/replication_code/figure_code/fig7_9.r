## The psre package must be installed first.  
## You can do this with the following code
# install.packages("remotes")
# remotes::install_github('davidaarmstrong/psre')
## load packages
library(tidyverse)
library(psre)
library(ggrepel)

## load data from psre package
data(wvs)

## filter to only waves 2 and 3 of the data. 
## turn democrat into a factor with the appropriate labels
## keep only the first instance where the country appears in the data
wvs0 <- wvs %>% 
  filter(wave %in% c(3,2)) %>% 
  mutate(democrat= factor(democrat, levels=1:2, 
                          labels=c("New Democracy", "Established Democracy"))) %>%
  group_by(country) %>% 
  arrange(wave) %>% 
  slice_head(n=1) 

## Fit different models for the two IVs 
## third-degree polynomial in gini
pmod <- lm(secpay ~ poly(gini_disp, 3) + democrat, data=wvs0)
## linear additive model 
lmod <- lm(secpay ~ gini_disp + democrat, data=wvs0)

## Select only the variables needed 
tmp <- wvs0 %>% dplyr::select(secpay, gini_disp, democrat, country) %>% na.omit()

## keep only the observations that we are not labelling 
tmp0 <- tmp %>% filter(!(country %in% c("Slovakia", "Czech Rep.", 
                                        "Chile", "Dominican Rep.", 
                                        "Russia", "Belarus")))

## Estimate the models to extract residuals
m0a <- lm(secpay ~ democrat, data=tmp)
m0b <- lm(gini_disp ~ democrat, data=tmp)
m0c <- lm(secpay ~ gini_disp, data=tmp)
m0d <- lm(I(democrat == "Established Democracy") ~ gini_disp, data=tmp)
## Secpay controlling for democracy
tmp$eyg <- residuals(m0a)
## gini controlling for democracy
tmp$exg <- residuals(m0b)
## secpay controlling for gini
tmp$eyd <- residuals(m0c)
## democrat controlling for gini
tmp$exd <- residuals(m0d)

## make a new variable, 'gltext' that has the country 
## names if the country is in the list below, and is 
## missing otherwise. 
tmp <- tmp %>% 
  ungroup %>% 
  mutate(gltext = case_when(
           country %in% c("Slovakia", "Czech Rep.", "Chile", 
                          "Belarus", "Russia", "Dominican Rep.") ~ country, 
           TRUE ~ NA_character_
         ))

## A. Model 7a: Gini Coefficient
ggplot(tmp, aes(x=exg, y=eyg)) + 
  #   geom_text(aes(label=.rownames)) + 
  geom_point(shape=1) + 
  geom_text_repel(aes(label=gltext)) + 
  geom_smooth(method="lm", se=FALSE, col="black") + 
  theme_classic() + 
  labs(x="GINI Residuals", y="Attitudes Toward Inequality Residuals")
ggsave("output/f7_9a.png", height=4.5, width=4.5, units="in", dpi=300)


## Proceed as above, but with different labelled cases.  The
## cases we decide to label will need to be identified by 
## trial and error. 

## B. Model 7a: Democratic History
tmp <- tmp %>% 
  ungroup %>% 
  mutate(dltext = case_when(
    country %in% c("Slovakia", "Czech Rep.") ~ country, 
    TRUE ~ NA_character_
  ))

ggplot(tmp, aes(x=exd, y=eyd)) + 
  #     geom_text(aes(label=.rownames)) + 
  geom_point(shape=1) +
  geom_text_repel(aes(label=dltext)) +
  geom_smooth(method="lm", se=FALSE, col="black") + 
  theme_classic() + 
  labs(x="Democratic History Residuals", y="Attitudes Toward Inequality Residuals")
ggsave("output/f7_9b.png", height=4.5, width=4.5, units="in", dpi=300)


## For the polynomial model, make the model matrix and 
## the response variable, y. 
Xp <- model.matrix(pmod)[,-1]
yp <- model.response(model.frame(pmod))
## regress the gini coefficient first-order term 
## and y on the other variables in the model and save residuals
rp1 <- lsfit(Xp[,-1], cbind(Xp[,1], yp), wt = rep(1, nrow(Xp)), intercept=TRUE)$residuals
## regress the gini coefficient second-order term 
## and y on the other variables in the model and save residuals
rp2 <- lsfit(Xp[,-2], cbind(Xp[,2], yp), wt = rep(1, nrow(Xp)), intercept=TRUE)$residuals
## regress the gini coefficient third-order term 
## and y on the other variables in the model and save residuals
rp3 <- lsfit(Xp[,-3], cbind(Xp[,3], yp), wt = rep(1, nrow(Xp)), intercept=TRUE)$residuals
## regress the democratic history term 
## and y on the other variables in the model and save residuals
dp1 <- lsfit(Xp[,-4], cbind(Xp[,4], yp), wt = rep(1, nrow(Xp)), intercept=TRUE)$residuals

## rename the variables in each of the residuals above
colnames(rp1) <- c("exgp1", "eygp1")
colnames(rp2) <- c("exgp2", "eygp2")
colnames(rp3) <- c("exgp3", "eygp3")
colnames(dp1) <- c("exdp", "eydp")

## put all data together
tmp <- cbind(tmp, cbind(rp1, rp2, rp3, dp1))

## Proceed as above

## C. Model 7c: Gini Coefficient
tmp <- tmp %>% 
  mutate(gltextp1 = case_when(
    country %in% c("Slovakia", "Czech Rep.", "Chile", 
                   "Belarus", "Peru", "Brazil") ~ country, 
    TRUE ~ NA_character_
  ))

ggplot(tmp, aes(x=exgp1, y=eygp1)) + 
  #     geom_text(aes(label=.rownames)) + 
  geom_point(shape=1) +
  geom_text_repel(aes(label=gltextp1)) +
  geom_smooth(method="lm", se=FALSE, col="black") + 
  theme_classic() + 
  labs(x="GINI Residuals", y="Attitudes Toward Inequality Residuals")
ggsave("output/f7_9c.png", height=4.5, width=4.5, units="in", dpi=300)

## D. Model 7c: Gini Coefficient Squared
tmp <- tmp %>% 
  mutate(gltextp2 = case_when(
    country %in% c( "Chile", "Peru", "Czech Rep.", "Brazil") ~ country, 
    TRUE ~ NA_character_
  ))

ggplot(tmp, aes(x=exgp2, y=eygp2)) + 
  #     geom_text(aes(label=.rownames)) + 
  geom_point(shape=1) +
  geom_text_repel(aes(label=gltextp2)) +
  geom_smooth(method="lm", se=FALSE, col="black") + 
  theme_classic() + 
  labs(x="GINI Squared Residuals", y="Attitudes Toward Inequality Residuals")
ggsave("output/f7_9d.png", height=4.5, width=4.5, units="in", dpi=300)

## E. Model 7c: Gini Coefficient Cubed. 
tmp <- tmp %>% 
  mutate(gltextp3 = case_when(
    country %in% c( "Slovakia", "Brazil") ~ country, 
    TRUE ~ NA_character_
  ))

ggplot(tmp, aes(x=exgp3, y=eygp3)) + 
  #     geom_text(aes(label=.rownames)) + 
  geom_point(shape=1) +
  geom_text_repel(aes(label=gltextp3)) +
  geom_smooth(method="lm", se=FALSE, col="black") + 
  theme_classic() + 
  labs(x="GINI Cubed Residuals", y="Attitudes Toward Inequality Residuals")
ggsave("output/f7_9e.png", height=4.5, width=4.5, units="in", dpi=300)

## F. Model 7c: Democratic History

tmp <- tmp %>% 
  mutate(dltextp = case_when(
    country %in% c( "Belarus", "Dominican Rep.") ~ country, 
    TRUE ~ NA_character_
  ))

ggplot(tmp, aes(x=exdp, y=eydp)) + 
  #     geom_text(aes(label=.rownames)) + 
  geom_point(shape=1) +
  geom_text_repel(aes(label=dltextp)) +
  geom_smooth(method="lm", se=FALSE, col="black") + 
  theme_classic() + 
  labs(x="Democratic History Residuals", y="Attitudes Toward Inequality Residuals")
ggsave("output/f7_9f.png", height=4.5, width=4.5, units="in", dpi=300)
