## Simulation for Measures of Association
## We do not present the graph in the book, but
## do talk about the results

library(tidyverse)
# library(DAMisc)


cutq <- function(x, quants = c(.3, .7), labs = NULL){
  x <- na.omit(x)
  qx <- quantile(x, probs=quants)
  qx <- c(min(x)-.01, qx, max(x)+.01)
  cx <- cut(x, breaks=qx)
  if(is.null(labs)){
    levels(cx) <- LETTERS[1:length(levels(cx))]
  }else{
    levels(cx) <- labs
  }
  cx
}
set.seed(202)
res <- NULL
rho <- runif(2500, .2, .8)
for(i in 1:2500){
  sig <- diag(2); sig[1,2] <- sig[2,1] <- rho[i]
  X <- MASS::mvrnorm(500, c(0,0), sig, empirical=TRUE)
  
  x1 <- cutq(X[,1], labs=c("A", "B", "C"))
  x2 <- cutq(X[,2], labs=c("M", "N", "O"))
  tab <- table(x1, x2)
  
  res <- rbind(res, c(
    DAMisc:::V(tab)["X-squared"], 
    DAMisc:::lambda(tab)[1], 
    DAMisc:::ord.gamma(tab), 
    DAMisc:::ord.somers.d(tab)$sd.symmetric, 
    DAMisc:::tau.b(tab)))
}
colnames(res) <- c("Cramer's V", "Lambda", "Gamma", "Somers' D", "Tau-b")
res <- as.data.frame(res)
res <- cbind(data.frame(r = rho), res)
res <- res %>% 
  pivot_longer(-r, names_to="measure", values_to="val")

ggplot(res, aes(x=r, y=val)) + 
  geom_point(shape=1, col="gray65") + 
  geom_abline(slope=1, intercept=0) + 
  facet_wrap(~measure) + 
  theme_bw() + 
  theme(panel.grid=element_blank()) + 
  labs(x="Correlation", y="Measure of Association")
ggsave("output/f4_0.png", height=4, width=6, units="in", dpi=300)
