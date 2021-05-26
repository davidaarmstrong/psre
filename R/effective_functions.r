### Auxiliary Functions for Effective Presentation of Statistical Results ###
### Dave Armstrong
### 2-28-2021

#' Kernel Density with Normal Density Overlay
#'
#' Calculates a kernel density estimate of the data along with confidence bounds.
#' It also computes a normal density and confidence bounds for the normal density
#' with the same mean and variance as the observed data.
#'
#' @param x A vector of values whose density is to be calculated
#' @param ... Other arguments to be passed down to \code{sm.density}.
#' @details The function is largely cribbed from the \pkg{sm} package by
#' Bowman and Azzalini
#' @return A named vector of scalar measures of fit
#' @author Dave Armstrong, A.W. Bowman and A. Azzalini
#' @references A.W> Bowman and A. Azzalini, R package sm: nonparametric smoothing methods
#' (verstion 5.6).
#'
#' @export
#' @importFrom stats density sd na.omit dnorm
#' @importFrom sm sm.density
normBand <- function (x, ...){
  x <- na.omit(x)
  d <- density(x, ...)
  s <- sm::sm.density(x, h=d$bw, model="none", eval.points=d$x, display="none")
  x.points <- d$x
  xbar <- mean(x, na.rm=TRUE)
  sx <- sd(x, na.rm=TRUE)
  hm <- d$bw
  dmean <- dnorm(x.points, xbar, sqrt(sx^2 + hm^2))
  dvar <- (dnorm(0, 0, sqrt(2 * hm^2)) * dnorm(x.points, xbar,
                                               sqrt(sx^2 + 0.5 * hm^2)) - (dmean)^2)/length(x)
  upper <- dmean + 2 * sqrt(dvar)
  lower <- dmean - 2 * sqrt(dvar)
  out <- data.frame(
    eval.points = x.points,
    obsden = s$estimate,
    lwd_od = s$lower,
    upr_od = s$upper,
    normden = dmean,
    lwr = lower,
    upr = upper)
  return(out)
}

#' Quantile Comparison Data
#'
#' Makes data that can be used in quantile comparison plots.
#'
#' @param x vector of values whose quantiles will be calculated.
#' @param distribution String giving the theoretical distribution
#' against which the quantiles of the observed data will be compared.
#' These need to be functions that have \code{q} and \code{d} functions
#' in R.  Defaults to "norm".
#' @param line String giving the nature of the line that should be drawn
#' through the points.  If "quartiles", the line is drawn connecting the 25th
#' and 75th percentiles.  If "robust" a robust linear model is used to fit
#' the line.
#' @param conf Confidence level to be used.
#' @param ... Other parameters to be passed down to the quantile function.
#'
#' @return A data frame with variables \code{x} observed quantiles,
#' \code{theo} the theoretical quantiles and \code{lwr} and \code{upr}
#' the confidence bounds.  The slope and intercept of the line running
#' through the points are returned as \code{a} and \code{b} as an
#' attribute of the data.a
#'
#' @export
#'
#' @importFrom stats qnorm dnorm quantile coef ppoints
#' @importFrom MASS rlm
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_segment geom_point theme_classic labs
#'
#' @examples
#' x <- rchisq(100, 3)
#' qqdf <- qqPoints(x)
#' a <- attr(qqdf, "ab")[1]
#' b <- attr(qqdf, "ab")[2]
#' l <- min(qqdf$theo) * b + a
#' u <- max(qqdf$theo) * b + a
#' library(ggplot2)
#' ggplot(qqdf, aes(x=theo, y=x)) +
#'   geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.15) +
#'   geom_segment(aes(x=min(qqdf$theo), xend=max(qqdf$theo), y = l, yend=u)) +
#'   geom_point(shape=1) +
#'   theme_classic() +
#'   labs(x="Theoretical Quantiles",
#'        y="Observed Quantiles")
qqPoints <- function (x,
                      distribution = "norm",
                      line = c("quartiles", "robust", "none"),
                      conf=.95, ...) {
  ## Taken from car:::qqPlot.default with some minor modifications
  line = match.arg(line)
  index <- seq(along = x)
  good <- !is.na(x)
  ord <- order(x[good])
  ord.x <- x[good][ord]
  q.function <- eval(parse(text = paste("q", distribution,
                                        sep = "")))
  d.function <- eval(parse(text = paste("d", distribution,
                                        sep = "")))
  n <- length(ord.x)
  P <- ppoints(n)
  z <- q.function(P, ...)
  #  points(z, ord.x, col = col, pch = pch, cex = cex)
  if (line == "quartiles" || line == "none") {
    Q.x <- quantile(ord.x, c(0.25, 0.75))
    Q.z <- q.function(c(0.25, 0.75), ...)
    b <- (Q.x[2] - Q.x[1])/(Q.z[2] - Q.z[1])
    a <- Q.x[1] - b * Q.z[1]
  }
  if (line == "robust") {
    coef <- coef(rlm(ord.x ~ z))
    a <- coef[1]
    b <- coef[2]
  }
  zz <- qnorm(1 - (1 - conf)/2)
  SE <- (b/d.function(z, ...)) * sqrt(P * (1 - P)/n)
  fit.value <- a + b * z
  upper <- fit.value + zz * SE
  lower <- fit.value - zz * SE
  outdf <- data.frame(
    x=ord.x,
    theo = z,
    lwr = lower,
    upr = upper
  )
  attr(outdf, "ab") <- c(a=a, b=b)
  return(outdf)
}

#' Transform Variables to Normality
#'
#' Uses the method proposed by Velez, Correa and Marmolejo-Ramos
#' to normalize variables using Box-Cox or Yeo-Johnson transformations.
#'
#' @param x Vector of values to be transformed to normality
#' @param start Positive value to be added to variable to ensure
#' all values are positive.  This follows the transformation of the variable
#' to have its minimum value be zero.
#' @param family Family of test - Box-Cox or Yeo-Johnson.
#' @param lams A vector of length 2 giving the range of values for the
#' transformation parameter.
#' @param combine.method String giving the method used to to combine
#' p-values from normality tests.
#' @param ... Other arguments, currently unimplemented.
#'
#' @return A scalar giving the optimal transformation parameter.
#'
#' @references
#' Velez Jorge I., Correa Juan C., Marmolejo-Ramos Fernando.  (2015)
#' "A new approach to the Box-Cox Transformation" Frontiers in Applied
#' Mathematics and Statistics.
#'
#' @export
#'
#' @importFrom stats na.omit shapiro.test
#' @importFrom car bcPower yjPower
#' @importFrom nortest lillie.test sf.test ad.test
#' @importFrom lawstat rjb.test
#' @importFrom normwhn.test normality.test1
transNorm <- function(x, start = .01, family=c("bc", "yj"), lams,
                      combine.method = c("Stouffer", "Fisher", "Average"), ...){
  family <- match.arg(family)
  cm <- match.arg(combine.method)
  x <- na.omit(x)
  if(any(x <=0) & family == "bc"){
    x <- x-min(x) + start
  }
  lambda <- seq(lams[1], lams[2], length=50)
  ptfun <- switch(family, bc = bcPower, yj = yjPower)

  trans_vals <- sapply(lambda, function(l)ptfun(x, lambda=l))
  novar <- which(apply(trans_vals, 2, sd) == 0)
  if(length(novar) > 0){
    trans_vals <- trans_vals[,-novar]
    lambda <- lambda[-novar]
  }
  trans_vals <- scale(trans_vals)
  p1 <- sapply(1:ncol(trans_vals), function(i)lillie.test(trans_vals[,i])$p.value)
  p2 <- sapply(1:ncol(trans_vals), function(i)sf.test(trans_vals[,i])$p.value)
  p3 <- sapply(1:ncol(trans_vals), function(i)ad.test(trans_vals[,i])$p.value)
  p4 <- sapply(1:ncol(trans_vals), function(i)shapiro.test(trans_vals[,i])$p.value)
  p5 <- sapply(1:ncol(trans_vals), function(i)rjb.test(trans_vals[,i])$p.value)
  sink(tempfile())
  p6 <- sapply(1:ncol(trans_vals), function(i)c(normality.test1(trans_vals[,i, drop=FALSE])[1,1]))
  sink()
  allp <- cbind(p1, p2, p3, p4, p5, p6)
  pcfun <- switch(cm, Stouffer = metap::sumz, Fisher = metap::sumlog, Average = metap::meanp)
  if(any(allp < 0.0000001)){
    allp[which(allp < 0.0000001, arr.ind= TRUE)] <- 0.0000001
  }
  p.combine <- apply(allp, 1, function(x)pcfun(x)$p)
  c(lambda = lambda[which.max(p.combine)])
}

#' Dot Plot with Leter Display
#'
#' Produces an dot plot with error bars along with a compact letter display
#'
#' @param fits Output from \code{ggpredict} from the \pkg{ggeffects}
#' @param letters A matrix of character strings giving the letters from a
#' compact letter display.  This is most often from a call to \code{cld} from the
#' \pkg{multcomp} package.
#'
#' @export
#' @importFrom ggplot2 geom_errorbarh ggplot_build aes_string geom_vline scale_x_continuous coord_cartesian ylab
#' @importFrom tibble as_tibble
#' @importFrom dplyr left_join
letter_plot <- function(fits, letters){
  if(!(all(c("x", "predicted", "conf.low", "conf.high") %in% names(fits))))stop("x, predicted, conf.low and conf.high need to be variables in the 'fits' data frame.")
  lmat <- letters
  g1 <- ggplot(fits, aes_string(y="x")) +
    geom_errorbarh(aes_string(xmin="conf.low", xmax="conf.high"),
                   height=0) +
    geom_point(aes_string(x="predicted"))
  p <- ggplot_build(g1)
  rgx <- p$layout$panel_params[[1]]$x.range
  diffrg <- diff(rgx)
  prty <- pretty(rgx, 4)
  if(prty[length(prty)] > rgx[2]){
    prty <- prty[-length(prty)]
  }
  labs <- as.character(prty)
  diffrg <- diff(range(c(rgx, prty)))
  firstlet <- max(c(max(prty), rgx[2])) + .075*diffrg
  vl <- max(rgx) + .0375*diffrg
  letbrk <- firstlet + (0:(ncol(lmat)-1))*.05*diffrg
  prty <- c(prty, letbrk)
  labs <- c(labs, LETTERS[1:ncol(lmat)])
  lmat <- t(apply(lmat, 1, function(x)x*letbrk))
  if(any(lmat == 0)){
    lmat[which(lmat == 0, arr.ind=TRUE)] <- NA
  }
  ldat <- as_tibble(lmat, rownames="x")
  dat <- left_join(fits, ldat)
  dat$x <- fits$x
  out <- ggplot(dat, aes_string(y="x")) +
    geom_errorbarh(aes_string(xmin="conf.low", xmax="conf.high"), height=0) +
    geom_point(aes_string(x="predicted"))
  obs_lets <- colnames(lmat)
  for(i in 1:length(obs_lets)){
    out <- out + geom_point(mapping=aes_string(x=obs_lets[i]), size=2.5)
  }
  out <- out + geom_vline(xintercept=vl, lty=2)+
    scale_x_continuous(breaks=prty,
                       labels=labs) +
    theme_classic() +
    coord_cartesian(clip='off') +
    ylab("")
  out
}

#' Calculate Simple Slopes
#'
#' Calculates Simple Slopes from an interaction between a categorical
#' and quantitative variable.
#'
#' @param mod A model object that contains an interaction between a
#' quantitative variable and a factor.
#' @param quant_var A character string giving the name of the quantitative
#' variable ine the interaction.
#' @param cat_var A character string giving the name of the factor
#' variable ine the interaction.
#'
#' @return A data frame giving the conditional partial effect
#' along with standard errors, t-statistics and p-values.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom stats pt vcov
#' @importFrom utils combn
#'
#' @export
simple_slopes <- function(mod, quant_var, cat_var){
  inds <- grep(quant_var, names(coef(mod)))
  me <- which(names(coef(mod)) == quant_var)
  inds <- c(me, setdiff(inds, me))
  levs <- mod$xlevels[[cat_var]]
  c1 <- matrix(0, nrow=length(levs), ncol=length(coef(mod)))
  rownames(c1) <- levs
  c1[1,inds[1]] <- 1
  for(i in 2:length(levs)){
    c1[i, inds[c(1,i)]] <- 1
  }
  est <-  c1 %*% coef(mod)
  v.est <- c1 %*% vcov(mod) %*% t(c1)
  df1 <- tibble(
    group = levs,
    slope = c(est),
    se = sqrt(diag(v.est)))
  df1 <- df1 %>% mutate(t = .data$slope/.data$se,
                        p = 2*pt(abs(.data$t),
                                     mod$df.residual,
                                     lower.tail=FALSE))
  combs <- combn(length(levs), 2)
  c2 <- matrix(0, ncol = ncol(combs), nrow=length(est))
  c2[cbind(combs[1,], 1:ncol(combs))] <- 1
  c2[cbind(combs[2,], 1:ncol(combs))] <- -1
  labs <- paste(levs[combs[1,]], levs[combs[2,]], sep="-")
  colnames(c2) <- labs
  df2 <- tibble(
    comp = labs,
    diff = c(t(c2) %*% est),
    se = sqrt(diag(t(c2) %*% v.est %*% c2)))
  df2 <- df2 %>% mutate(
    t = .data$diff/.data$se,
    p = 2*pt(abs(.data$t), mod$df.residual, lower.tail=FALSE))
  res <- list(est = df1, comp=df2, v=v.est)
  class(res) <- "ss"
  res
}

#' Print Method for Simple Slopes
#'
#' Prints the results of the Simple Slopes function
#'
#' @param x An object of class \code{ss}.
#' @param ... Other arguments passed down to \code{print}
#'
#' @return Printed output
#'
#' @export
#' @method print ss
print.ss <- function(x, ...){
  cat("Simple Slopes:\n")
  print(x$est, ...)
  cat("\nPairwise Comparisons:\n")
  print(x$comp, ...)
}

#' Compact Letter Display for Simple Slopes
#'
#' Calculates a letter matrix for a simple-slopes output.
#'
#' @param x An object of class `ss`
#' @param level Confidence level used for the letters.
#' @param ... Other arguments to be passed to generic function.
#'
#' @return A compact letter matrix
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange select pull
#' @importFrom tidyr separate
#' @importFrom utils getFromNamespace
#' @importFrom multcomp cld
#' @importFrom rlang .data
#'
#' @export
#' @method cld ss
cld.ss <- function(x, level=.05, ...){
  ord <- x$est %>% arrange(.data$slope) %>% select("group") %>% pull
  signif <- x$comp$p < level
  comps <- x$comp %>% select("comp") %>% separate(.data$comp, sep="-", into=c("g1", "g2")) %>% as.matrix()
  rownames(comps) <- x$comp$comp
  ia <- getFromNamespace("insert_absorb", "multcomp")
  ia(signif, comps=comps, lvl_order = ord)$LetterMatrix
}

#' BCn Power Transformation of Variable
#'
#' @param x variable to be transformed
#' @noRd
#'
#' @importFrom car powerTransform bcnPower
trans_fun <- function(x){
  p <- powerTransform(x, family="bcnPower")
  trans <- bcnPower(x, p$lambda, gamma=p$gamma)
  trans
}

#' Association Function
#'
#' Calculates the R-squared from a LOESS regression of
#' y on x.  Can be used with \code{outer} to produce the
#' a non-parametric correlation matrix.
#'
#' @param xind column index of the x-variable
#' @param yind column index of the y-variable
#' @param data data frame from which to pull the variables.
#'
#' @return a squared correlation.
#'
#' @importFrom fANCOVA loess.as
#' @importFrom stats cor
#'
#' @export
assocfun <- function(xind,yind, data){
  d <- data.frame(x=data[,xind],
                  y=data[,yind])
  d <- na.omit(d)
  l <- loess.as(d$x, d$y, criterion="gcv")
  cor(d$y, l$fitted, use="pair")^2
}

#' Linear Scatterplot Array
#'
#' Produces a linear scatterplot array with marginal histograms
#'
#' @param formula Formula giving the variables to be plotted.
#' @param xlabels Vector of character strings giving the labs of
#' variables to be used in place of the variable names.
#' @param ylab Character string giving y-variable label to be
#' used instead of variable name.
#' @param data A data frame that holds the variables to be plotted.
#' @param ptsize Size of points. 
#' @param ptshape Shape of points.
#' @param ptcol Color of points.

#'
#' @importFrom ggplot2 geom_smooth facet_wrap theme_bw theme
#' element_blank element_text geom_histogram element_line coord_flip
#' @importFrom grid rectGrob gpar
#' @importFrom cowplot plot_grid
#' @importFrom stats as.formula terms
#'
#' @return A \code{cowplot} object.
#'
#' @export


lsa <- function(formula, xlabels=NULL, ylab = NULL, data,
                ptsize=1, ptshape=1, ptcol="gray65"){
  if (!attr(terms(as.formula(formula)), which = 'response'))
    stop("No DV in formula.\n")
  avf <- all.vars(formula)
  tmp <- data %>%
    select(avf)
  dv <- avf[1]
  ivs <- avf[-1]
  if(is.null(ylab))ylab <- dv
  if(is.null(xlabels))xlabels <- ivs
  if(length(ivs) != length(xlabels))stop("Labels and #IVs are not the same\n")
  slist <- hlist <- list()
  for(i in 1:length(ivs)){
    if(i == 1){
      slist[[i]] <- ggplot(tmp, aes_string(y=dv, x=ivs[i])) +
      geom_point(size=ptsize, shape=ptshape, col=ptcol) +
      geom_smooth(method="loess", size=.5, se=FALSE, col="black") +
      facet_wrap(as.formula(paste0('~"', xlabels[i], '"')))+
      theme_bw() +
      theme(panel.grid=element_blank()) +
      labs(x="", y=ylab[1])

      hlist[[i]] <- ggplot(tmp, aes_string(x=ivs[i])) +
      geom_histogram(fill="gray75", col="white", bins=15) +
      theme(panel.grid=element_blank(),
            panel.background = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x=element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_text(colour="transparent"),
            axis.text.y=element_text(colour="transparent"),
            axis.ticks.y = element_line(colour="transparent")) +
      labs(y="Histogram")
    }else{
      slist[[i]] <- ggplot(tmp, aes_string(y=dv, x=ivs[i])) +
        geom_point(size=ptsize, shape=ptshape, col=ptcol) +
        geom_smooth(method="loess", size=.5, se=FALSE, col="black") +
        facet_wrap(as.formula(paste0('~"', xlabels[i], '"')))+
        theme_bw() +
        theme(panel.grid=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank()) +
        labs(x="", y="Y")
      hlist[[i]] <- ggplot(tmp, aes_string(x=ivs[i])) +
        geom_histogram(fill="gray75", col="white", bins=15) +
        theme(panel.grid=element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_blank(),
              axis.title.x=element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y = element_blank())

    }
  }
    hlist[[(length(ivs)+1)]] <- rectGrob(gp=gpar(col="white")) # make a white spacer grob
    slist[[(length(ivs)+1)]] <- ggplot(tmp, aes_string(x=dv)) +
      geom_histogram(fill="gray75", col="white", bins=15) +
      theme(panel.grid=element_blank(),
            panel.background = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y=element_blank(),
            axis.text.x=element_text(colour="transparent"),
            axis.ticks.x = element_line(colour="transparent"),
            axis.title.x=element_blank()
      ) +
      labs(x="", y="") +
      coord_flip()

  l <- c(hlist, slist)
  l[["nrow"]] = 2
  l[["rel_heights"]] = rel_heights=c(1,5)
  l[["rel_widths"]] = rel_widths=c(1.25, rep(1, length(ivs)-1), .5)
  do.call(plot_grid, l)
}


#' Residual-Residual Plot
#'
#' Produces a linear scatterplot array with marginal histograms.
#' The plots have OLS regression lines and a 45-degree line.
#'
#' @param formula Formula giving the variables to be plotted.
#' @param xlabels Vector of character strings giving the labs of
#' variables to be used in place of the variable names.
#' @param ylab Character string giving y-variable label to be
#' used instead of variable name.
#' @param data A data frame that holds the variables to be plotted.
#' @param return A string identify what to return.  If \sQuote{grid}, 
#' then a \code{cowplot} object is returned with all plots printed.  
#' If \sQuote{grobs} then a list with all of the individual ggplots/grobs
#' is returned. 
#' @param ptsize Size of points. 
#' @param ptshape Shape of points.
#' @param ptcol Color of points.
#'
#' @importFrom ggplot2 geom_smooth facet_wrap theme_bw theme
#' element_blank element_text geom_histogram element_line coord_flip
#' @importFrom grid rectGrob gpar
#' @importFrom cowplot plot_grid
#' @importFrom stats as.formula terms
#'
#' @return A \code{cowplot} object.
#'
#' @export
rrPlot <- function(formula, xlabels=NULL, ylab = NULL, 
                   data, return = c("grid", "grobs"), 
                   ptsize = 1, ptshape=1, ptcol="gray65"){
  ret <- match.arg(return)
  if (!attr(terms(as.formula(formula)), which = 'response',
            return ))
    stop("No DV in formula.\n")
  avf <- all.vars(formula)
  tmp <- data %>%
    select(avf)
  dv <- avf[1]
  ivs <- avf[-1]
  if(is.null(ylab))ylab <- dv
  if(is.null(xlabels))xlabels <- ivs
  if(length(ivs) != length(xlabels))stop("Labels and #IVs are not the same\n")
  slist <- hlist <- list()
  for(i in 1:length(ivs)){
    if(i == 1){
      slist[[i]] <- ggplot(tmp, aes_string(y=dv, x=ivs[i])) +
        geom_point(size=ptsize, shape=ptshape, col=ptcol) +
        geom_smooth(method="lm", size=.5, se=FALSE, col="black") +
        geom_abline(slope=1, intercept=0, lty=3) +
        facet_wrap(as.formula(paste0('~"', xlabels[i], '"')))+
        theme_bw() +
        theme(panel.grid=element_blank()) +
        labs(x="", y=ylab[1])

      hlist[[i]] <- ggplot(tmp, aes_string(x=ivs[i])) +
        geom_histogram(fill="gray75", col="white", bins=15) +
        theme(panel.grid=element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_blank(),
              axis.title.x=element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.y = element_text(colour="transparent"),
              axis.text.y=element_text(colour="transparent"),
              axis.ticks.y = element_line(colour="transparent")) +
        labs(y="Histogram")
    }else{
      slist[[i]] <- ggplot(tmp, aes_string(y=dv, x=ivs[i])) +
        geom_point(size=ptsize, shape=ptshape, col=ptcol) +
        geom_smooth(method="lm", size=.5, se=FALSE, col="black") +
        geom_abline(slope=1, intercept=0, lty=3) +
        facet_wrap(as.formula(paste0('~"', xlabels[i], '"')))+
        theme_bw() +
        theme(panel.grid=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank()) +
        labs(x="", y="Y")
      hlist[[i]] <- ggplot(tmp, aes_string(x=ivs[i])) +
        geom_histogram(fill="gray75", col="white", bins=15) +
        theme(panel.grid=element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_blank(),
              axis.title.x=element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y = element_blank())

    }
  }
  hlist[[(length(ivs)+1)]] <- rectGrob(gp=gpar(col="white")) # make a white spacer grob
  slist[[(length(ivs)+1)]] <- ggplot(tmp, aes_string(x=dv)) +
    geom_histogram(fill="gray75", col="white", bins=15) +
    theme(panel.grid=element_blank(),
          panel.background = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_text(colour="transparent"),
          axis.ticks.x = element_line(colour="transparent"),
          axis.title.x=element_blank()
    ) +
    labs(x="", y="") +
    coord_flip()

  l <- c(hlist, slist)
  l[["nrow"]] = 2
  l[["rel_heights"]] = rel_heights=c(1,5)
  l[["rel_widths"]] = rel_widths=c(1.25, rep(1, length(ivs)-1), .5)
  if(ret == "grid"){
    do.call(plot_grid, l)
  }else{
    return(grobs=l, data=tmp, dv = dv, ivs=ivs)
  }
}



#' Caption Grob
#'
#' Create a caption grob
#'
#' @param lab Text giving the caption text.
#' @param x Scalar giving the horizontal position of the label in \code{[0,1]}.
#' @param y Scalar giving the vertical position of the label in \code{[0,1]}.
#' @param hj Scalar giving horizontal justification parameter.
#' @param vj Scalar giving vertical justification parameter.
#' @param cx Character expansion factor
#' @param fs Font size
#' @param ft Font type
#'
#' @return A text grob.
#'
#' @importFrom grid textGrob unit
#'
#' @export
caption <- function(lab, x=.5, y=1, hj=.5, vj=1, cx=1, fs=12, ft="Arial"){
  textGrob(label=lab,
           x=unit(x, "npc"), y=unit(y, "npc"),
           hjust=hj, vjust=vj,
           gp=gpar(fontsize=fs, fontfamily=ft))

}

#' Bootstrap Importance Function
#'
#' Function to calculate bootstrap measures of importance.
#' This function must be passed to the \code{boot} function.
#'
#' @param data A data frame
#' @param inds Indices to be passed into the function.
#' @param obj An object of class \code{lm}.
#'
#' @return A vector of standard deviation of predictions for
#' each term in the model.
#'
#' @importFrom stats predict update
#'
#' @export
boot_imp <- function(data, inds, obj){
  tmp <- update(obj, data=data[inds, ])
  apply(predict(tmp, type="terms"), 2, sd)
}

#' Absolute Importance Measure
#'
#' Calculates absolute importance along the lines consistent with
#' relative importance as defined by Silber, Rosenbaum and Ross (1995)
#'
#' @param obj Model object, must be able to use \code{predict(obj, type="terms")}.
#' @param data A data frame used to estiamte the model.
#' @param boot Logical indicating whether bootstrap confidence intervals should
#' be produced and included.
#' @param R If \code{boot=TRUE}, the number of bootstrap samples to be used.
#' @param level Cofidence level used for the confidence interval.
#' @param ci_method Character string giving the method for calculating the
#' bootstrapped confidence interval.
#' @param ... Other arguments being passed down to \code{boot}.
#'
#' @return A data frame of importance measures with optimal bootstrapped confidence intervals.
#'
#' @importFrom boot boot boot.ci
#'
#' @references Silber, J. H., Rosenbaum, P. R. and Ross, R N (1995) Comparing the Contributions of Groups of Predictors: Which Outcomes Vary with Hospital Rather than Patient Characteristics? JASA 90, 7–18.
#'
#' @export
srr_imp <- function(obj,
                    data,
                    boot=TRUE,
                    R=250,
                    level = .95,
                    ci_method=c("perc", "norm", "bca"),
                    ...){
  cim <- match.arg(ci_method)
  trms <- predict(obj, type="terms")
  out <- do.call(data.frame, list(importance=apply(trms, 2, sd)))
  out$var <- rownames(out)
  rownames(out) <- NULL
  out <- out[,c("var", "importance")]
  if(boot){
    b_out <- boot(data, boot_imp, R=R, obj=obj, ...)
    b_ci <- t(sapply(1:ncol(b_out$t), function(i)boot.ci(b_out, conf=level, type=cim, index=i)[[4]]))[,4:5]
    colnames(b_ci) <- c("lwr", "upr")
    out <- cbind(out, b_ci)
  }
  return(out)
}

#' Importace Measure for Generalized Linear Models
#'
#' Calculates importance along the lines of Greenwell et al (2018)
#' using partial dependence plots.
#'
#' @param obj Model object, must be able to use \code{predict(obj, type="terms")}.
#' @param data A data frame used to estiamte the model.
#' @param varname Character string giving the name of the variable whose importance
#' will be calculated.
#' @param level Cofidence level used for the confidence interval.
#' @param ci_method Character string giving the method for calculating the
#' confidence interval - normal or percentile.
#' @param ... Other arguments being passed down to \code{aveEffPlot} from the \pkg{\link{DAMisc}} package.
#'
#' @return A data frame of importance measures with optimal bootstrapped confidence intervals.
#'
#' @references Greenwell, Brandon M., Bradley C. Boehmke and Andrew J. McCarthy.  (2018). “A Simple and Effective Model-Based Variable Importance Measure.”  arXiv1805.04755 [stat.ML]
#'
#' @importFrom DAMisc aveEffPlot
#'
#' @export
glmImp <- function(obj,
                   varname,
                   data,
                   level=.95,
                   ci_method = c("perc", "norm"),
                   ...){
  cit <- match.arg(ci_method)
  a <- (1-level)/2
  fac <- is.factor(data[[varname]])
  eff <- aveEffPlot(obj, varname, data, return = "sim", ...)
  re <- apply(eff$sim, 1, sd)
  ce <- colMeans(eff$sim)
  if(!fac){
    e <- sd(ce)
  }else{
    e <- diff(range(ce))/4
  }
  if(cit == "norm"){
    outci <- e + qnorm(c(a, 1-a))*sd(re)
  }else{
    outci <- quantile(re, probs=c(a, 1-a))
  }
  res <- data.frame(imp = e, lwr = outci[1], upr = outci[2])
  res
}

#'  Internal function to be used by optCL
#'  
#'  This is an internal function to be used with optCL, 
#'  but is documented here for completeness. 
#'  
#'  @param b A vector of coefficients to be tested. 
#'  @param v A variance-covariance matrix for \code{b}. 
#'  @param vt An optional vector of variances (like quasi-variances)
#'  that can be used to make the confidence intervals. 
#'  @param eg A two-column matrix giving the index numbers of the 
#'  simple contrasts.  It should be that \code{eg[,1]} is smaller than 
#'  \code{eg[,2]}.  
#'  @param resdf Residual degrees of freedom to be used to make t-statistics. 
#'  @param alpha p-value used to reject the null hypothesis. 
#'  @param clev Candidate confidence level for the optimised visual testing
#'  search. 
#'  
#' @noRd
#' 
#' @importFrom dplyr filter 
#' @importFrom stats model.matrix qt
#' @importFrom ggplot2 geom_abline
make_fdat <- function(b, v, vt, eg, resdf=Inf, alpha=.05, clev){
  vi <- diag(v)
  fdat <- tibble(
    cat1 = eg[,1], 
    cat2 = eg[,2], 
    b1 = b[eg[,1]], 
    b2 = b[eg[,2]], 
    v1 = vi[eg[,1]], 
    v2 = vi[eg[,2]], 
    vt1 = vt[eg[,1]], 
    vt2 = vt[eg[,2]], 
    cov12 = v[cbind(eg[,1], eg[,2])])
  fdat <- fdat %>% 
    mutate( 
      comp_var = .data$v1 + .data$v2 - 2*.data$cov12, 
      diff = .data$b1-.data$b2) %>% 
    mutate(
    t = .data$diff/sqrt(.data$comp_var),
    p = 2*pt(abs(.data$t), resdf, lower.tail=FALSE), 
    sig = as.numeric(.data$p < alpha), 
    lb1 = .data$b1-qt(1-((1-clev)/2), resdf)*sqrt(.data$vt1), 
    ub1 = .data$b1+qt(1-((1-clev)/2), resdf)*sqrt(.data$vt1), 
    lb2 = .data$b2-qt(1-((1-clev)/2), resdf)*sqrt(.data$vt2), 
    ub2 = .data$b2+qt(1-((1-clev)/2), resdf)*sqrt(.data$vt2))
  fdat <- fdat %>% 
    rowwise %>% mutate(
      olap = case_when(
        .data$diff > 0 ~ as.numeric(.data$lb1 < .data$ub2), 
        .data$diff < 0 ~ as.numeric(.data$ub1 > .data$lb2), 
        TRUE ~ 0), 
      crit = case_when(
        .data$olap == 1 & .data$diff > 0 ~ (.data$ub2 - .data$lb1)^2, 
        .data$olap == 1 & .data$diff < 0 ~ (.data$ub1 - .data$lb2)^2, 
        .data$olap == 0 & .data$diff > 0 ~ (.data$lb1 - .data$ub2)^2, 
        .data$olap == 0 & .data$diff < 0 ~ (.data$lb2 - .data$ub1)^2, 
      ))
  na.omit(fdat)
}  

#' Calculate the Optimal Visual Testing Confidence Level
#' 
#' Calculates the Optimal Visual Testing (OVT) confidence level.  The
#' OVT level is a level you can use to make confidence intervals such that
#' the overlapping (or non-overlapping) of confidence intervals preserves
#' the pairwise testing results. That is, statistically different 
#' estimates have confidence intervals that do not overlap and statistically
#' indistinguishable intervals have confidence intervals that do overlap. 
#' It does not always work perfectly, but it generally results in fewer
#' inferential errors than the nominal level. 
#' 
#' @param obj A model object, on which \code{coef} and \code{vcov} can be called. 
#' Either \code{obj} and \code{varname} or \code{b} and \code{v} must be specified.
#' @param varname The name of a variable whose coefficients will be used. 
#' @param b Optional vector of coefficients to be passed into the function.  
#' it overrides the coefficients in \code{obj}. Either \code{obj} and 
#' \code{varname} or \code{b} and \code{v} must be specified.
#' @param v Optional variance-covariance matrix.  This can be specified 
#' even if \code{obj} and \code{varname} are specified.  It replaces the
#' variance-covaraince matrix from the model. 
#' @param resdf If only \code{b} and \code{v} are passed in, this gives 
#' the residual degrees of freedom for the t-statistics. 
#' @param level The confidence level to use for testing. 
#' @param quasi_vars An optional vector of quasi-variances that will be
#' used to make the confidence intervals. 
#' @param add_ref If \code{obj} and \code{varname} are passed in, 
#' an optional 0 is added to the front of the vector of coefficients, 
#' along with a leading row and column of zeros on the variance-covariance
#' matrix to represent the reference category. 
#' @param grid_range The range of values over which to do the grid search. 
#' @param grid_length The number of values in the grid.  
#' 
#' @return A list with the following elements: 
#' \describe{
#'   \item{opt_levels}{The optimal confidence levels that all have 
#'   identical minimal error rates. }
#'   \item{opt_errors}{The proportion of errors across all simple contrasts.}
#'   \item{lev_errors}{The proportion of errors made at the nominal 
#'   significance level.}
#'   \item{tot_comps}{The total number of comparisons}
#'   \item{lev_dat}{If there are inferential errors at the nominal level, 
#'   this is a data frame that has all of the information about which 
#'   comparisons are not appropriately represented by the overlaps in 
#'   confidence intervals.}
#'   \item{err_dat}{If there are inferential errors at the optimal level, 
#'   this is a data frame that has all of the information about which 
#'   comparisons remain not appropriately represented by the overlaps in 
#'   optimized confidence intervals.}
#' }
#' 
#' @importFrom dplyr rowwise case_when ungroup summarise
#' @export
optCL <- function(obj=NULL, varname=NULL, b=NULL, v=NULL, 
                  resdf = Inf, level=.95, 
                  quasi_vars = NULL, 
                  add_ref = TRUE, 
                  grid_range = c(.75, .99), 
                  grid_length=100){
  if(is.null(obj) & is.null(varname)){
    if(is.null(b) | is.null(v))stop("Both b and v must be provided if obj and varname are NULL.\n")
  }
  if(is.null(b) | is.null(v)){
    if(is.null(obj) | is.null(varname))stop("Both obj and varname must be provided if b or v is NULL.\n")
  }
  resdf <- ifelse(is.null(obj), resdf, obj$df.residual)
  res <- crit <- rep(NA, length=grid_length)
  alpha <- 1-level
  grid_pts <- seq(grid_range[1], grid_range[2], length=grid_length)
  grid_pts <- sort(unique(c(level, grid_pts)))
  if(is.null(b)){
    X <- model.matrix(obj)
    assgn <- attr(X, "assign")
    tl <- attr(terms(obj), "term.labels")
    varind <- which(tl == varname)
    inds <- which(assgn == varind)
    if(length(inds) == 0){
      stop("varname not a term label in model matrix.\n")
    }
    if(add_ref){
      b <- c(0, coef(obj)[inds])
    }
    if(is.null(v)){
      v <- vcov(obj)[inds, inds]
      if(add_ref){
        v <- cbind(0, rbind(0, v))  
      }
    }
  }
  if(length(b) != ncol(v))stop("Length of b and dimension of v must be the same.\n")
  if(nrow(v) != ncol(v))stop("v must be square.\n")
  if(is.null(b) & !is.null(v))stop("If getting v from the model, you must also get v from the model.\n")
  eg <- expand.grid(b1 = 1:length(b), b2 = 1:length(b))
  eg <- eg %>% filter(.data$b1 < .data$b2)
  vi <- diag(v)
  vt <- {if(is.null(quasi_vars)){
    vi
  }else{
    quasi_vars
  }}
  for(i in 1:length(grid_pts)){
    fd <- make_fdat(b,v,vt, eg, resdf, alpha, grid_pts[i])  
    fd <- fd %>% na.omit()
    res[i] <- fd %>% 
      ungroup %>% 
      summarise(err = mean(.data$olap == .data$sig)) %>% 
      pull()
    crit[i] <- fd %>% 
      ungroup %>% 
      summarise(err = sum(.data$crit)) %>% 
      pull()
  }
  w <- which(res == min(res))
  if(min(res) > 0){
    ret_dat <- make_fdat(b,v,vt, eg, resdf, alpha, grid_pts[w[1]])  
    ret_dat <- ret_dat %>% 
      filter(.data$olap == .data$sig)
  }
  else{
    ret_dat <- NULL
  }
  if(res[which(grid_pts == level)] > 0){
    lev_dat <- make_fdat(b,v,vt, eg, resdf, alpha, level)  
    lev_dat <- lev_dat %>% 
      filter(.data$olap == .data$sig)
  }
  else{
    lev_dat <- NULL
  }
  return(list(opt_levels = grid_pts[w], 
              crit = crit[w], 
              opt_errors = min(res), 
              lev_errors = res[which(grid_pts == level)], 
              tot_comps = nrow(fd), 
              lev_dat = lev_dat, 
              err_dat = ret_dat))
}



loess.aic <- function (x) {
  ## Written by Michael Friendly, with help from John Fox
  ## https://stat.ethz.ch/pipermail/r-help/2005-November/082853.html
  
  if (!(inherits(x,"loess"))) stop("Error: argument must be a loess object")
  # extract values from loess object
  span <- x$pars$span
  n <- x$n
  traceL <- x$trace.hat
  sigma2 <- x$s^2
  delta1 <- x$one.delta
  delta2 <- x$two.delta
  enp <- x$enp
  
  aicc <- log(sigma2) + 1 + 2* (2*(traceL+1)) / (n-traceL-2)
  aicc1<- n*log(sigma2) + n* ((delta1/delta2)*(n+enp)/(delta1^2/delta2)-2 )
  gcv  <- n*sigma2 / (n-traceL)^2
  result <- list(span=span, aicc=aicc, aicc1=aicc1, gcv=gcv)
  return(result)
}

#' Heatmap Fit Plot using GGplot
#' 
#' Makes a Heatmap Fit plot (Esary and Pierce, 2012) using
#' GGPlot rather than lattice that the \code{heatmapFit} package
#' uses. 
#' 
#' @param observed Vector of observe (0/1) values used in a 
#' binary regression model. 
#' @param prob Vector of predicted probabilities from the model 
#' with \code{observed} as the dependent variable. 
#' @param span Optional span parameter to be passed in.  If 
#' \code{NULL}, AICc will be used to find the appropriate 
#' span for the loess smooth. 
#' @param method Method for making the line - LOESS or GAM (from the \code{mgcv} package.)
#' @param nbin Number of bins for the histogram. 
#' @param R Number of boostrap resamples
#' @param ... Currently unimplemented. 
#' 
#' @return Two ggplots - the main heatmap Fit plot and a 
#' histogram that can be included as a marginal density. 
#' 
#' @importFrom stats loess rbinom
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom ggplot2 geom_line
#' @importFrom mgcv gam
#' @export
#' 
gg_hmf <- function(observed, prob, method = c("loess", "gam"), 
                   span=NULL, nbin=20, R=1000, ...){
## TODO: Make consistent with heatmap.fit
  
    method <- match.arg(method)
  tmp <- na.omit(data.frame(
    yobs=observed, 
    prob = prob))
  if(method == "loess"){
    if(is.null(span)){
      smooth.err<-function(span.arg){
        ## Written by Justin Esarey in the heatmapFit package
        ok<-T
        plot.model<-withCallingHandlers(tryCatch(loess(yobs~prob, data=tmp, degree=1, weights = rep(1, nrow(tmp)), 
                                                       span=span.arg, control = loess.control(trace.hat="exact"))),  
                                        warning = function(w){ok<<-F; invokeRestart("muffleWarning")})
        if(ok==T){return(eval(parse(text=paste("loess.aic(plot.model)$aicc", sep=""))))}
        if(ok==F){return(2e10)}
      }  
      
      # do the optimization, set the span argument to the optimal value
      spn<-optimize(f=smooth.err, interval=c(0.01, 0.99))$minimum
    }else{
      spn <- span
    }
  message(paste0("LOESS span = ", round(spn, 3), "\n\n"))
  lo <- loess(yobs ~ prob, degree=1, data=tmp, span=spn)
  pred <- predict(lo)
  }else{
    gm <- gam(yobs ~ s(prob), data=tmp)
    pred <- predict(gm)
  }
  unx <- seq(from=min(pred), to=max(pred), length=250)
  est.dat <- data.frame(
    y=NA, 
    prob=tmp$prob) ## or tmp$prob
  pred.dat <- data.frame(
    y = 0, 
    prob = unx
  )
  pred.y <- pred.y.obs <- NULL
  cat("Generating Bootstrap Predictions ...\n")
  pb <- txtProgressBar(min = 0, max = R, style = 3)
  for(i in 1:R){
    setTxtProgressBar(pb, i)
    est.dat$y <- ifelse(runif(nrow(tmp), min = 0, max = 1) < pred, 1, 0)  
    if(method == "loess"){
      lo <- loess(y ~ prob, data=est.dat, degree=1, span=spn)  
    }else{
      lo <- gam(y ~ s(prob), data=est.dat)
    }
    py <- predict(lo, newdata=pred.dat)
    pyo <- predict(lo, newdata=est.dat)
    py <- case_when(py < 0 ~ 0, py > 1 ~ 1, TRUE ~ py)
    pyo <- case_when(pyo < 0 ~ 0, pyo > 1 ~ 1, TRUE ~ pyo)
    pred.y <- cbind(pred.y, py)
    pred.y.obs <- cbind(pred.y.obs, pyo)
  }
  ap <- apply(pred.y.obs, 2, function(x)(2*(x < pred) + (x == pred)))
  pvals <- apply(ap, 1, function(x)(sum(x)/2)/R)
  pct_out <- sum(pvals < .1 | pvals > .9)/R
  ci1 <- t(apply(pred.y, 1, function(x)quantile(x, c(.1,.9), na.rm=TRUE)))
  if(method=="loess"){
    lo <- loess(yobs ~ prob, data=tmp, degree=1, span=spn)  
  }else{
    lo <- gam(yobs ~  s(prob), data=tmp)
  }
  plot.hm <-tibble(
    x=unx, 
    fit=predict(lo, newdata=data.frame(prob=unx)), 
    lwr = ci1[,1], 
    upr = ci1[,2], 
  )
  
  # pct_out <- plot.hm %>% 
  #   mutate(tot = sum(.data$n)) %>% 
  #   filter(.data$x < .data$lwr | .data$x > .data$upr) %>% 
  #   summarise(pct = sum(.data$n)/mean(.data$tot)) %>% 
  #   pull()
  
  if(!is.finite(pct_out))pct_out <- 0
  
  hm1 <- ggplot(plot.hm) + 
    geom_ribbon(aes(x=.data$x, ymin = .data$lwr, ymax=.data$upr), alpha=.25) + 
    geom_line(aes(x=.data$x, y=.data$fit)) + 
    geom_abline(intercept=0, slope=1, linetype=2) + 
    theme_classic() + 
    labs(x=paste0("Model Predicted, Pr(y=1)\n(", 
                  sprintf("%.0f", pct_out*100), "% Outside of CI)"), 
         y="Smoothed Empirical Pr(y=1)")
  
  hm1_dens <- ggplot(tmp, aes(x=.data$prob)) + 
    geom_histogram(fill="gray75", col="white", bins=nbin) +
    theme(panel.grid=element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(colour="transparent"),
          axis.text.y= element_text(colour="transparent"),
          axis.ticks.y = element_line(colour="transparent")) 
  
  return(list(hist = hm1_dens, main = hm1))
}


#' Hybrid Plot for DFBETAS
#' 
#' Plots a hybrid histogram, dot plot for DFBETAS.  A histogram is plotted
#' for the observations below \code{cutval}.  Observations above \code{cutval}
#' are plotted and labelled with individual points. 
#' 
#' @param data A data frame of DFBETAS values
#' @param varname The name of the variable to plot
#' @param label Name of variable that holds the labels that will go with the points
#' @param cutval The value that separates the histogram from the individual points. 
#' @param binwidth The bin width for the histogram part of the display. 
#' @param xlab Label to put on the x-axis. 
#' @param ylab Label to put on the y-axis. 
#' @param xrange Alternative range to plot on the x-axis. 
#' @param yrange Alternative range to plot on y-axis
#' @param nudge_x Vector of values to nudge labels horizontally.
#' @param nudge_y Vector of values to nudge labels vertically.
#' 
#' @importFrom stats dfbetas
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 coord_cartesian
#' 
#' @return A ggplot. 
#' 
#' @export
#' 
#' @examples 
#' 
#' data(wvs)
#' wvs <- na.omit(wvs[,c("country", "secpay", "gini_disp", "democrat")])
#' lmod <- lm(secpay ~ gini_disp + democrat, data=wvs)
#' dba <- dfbetas(lmod)
#' dbd <- wvs
#' dbd$dfb_ginil <- dba[,2]^2
#' dbd$dfb_democl <- dba[,3]^2
#' dfbhist(dbd, "dfb_ginil", "country")
#' 
dfbhist <- function(data, varname, label, cutval=.25, binwidth=.025, xlab="DFBETAS", ylab="Frequency", 
                    xrange = NULL, yrange=NULL, nudge_x=NULL, nudge_y=NULL){
  t1 <- data %>% filter(.data[[varname]] <= cutval)
  t2 <- data %>% filter(.data[[varname]] > cutval)
  
  g1 <- ggplot() + 
    theme_classic() + 
    labs(x=xlab, y=ylab)
  if(nrow(t1) > 0){
    g1 <- g1 + 
      geom_histogram(data=t1, aes_string(x=varname), col="white", boundary=0, 
                     binwidth=binwidth, position="identity")
  }
  if(nrow(t2) > 0){
    if(is.null(nudge_x))nudge_x <- rep(0, nrow(t2))
    if(is.null(nudge_y))nudge_y <- rep(0, nrow(t2))
    g1 <- g1 + geom_point(data=t2, aes_string(x=varname, y="0")) + 
      geom_text_repel(data=t2, aes_string(x=varname, y="0", label=label),
                      nudge_x = nudge_x, nudge_y = nudge_y) 
  }
  g1 + coord_cartesian(xlim=xrange, ylim=yrange)
}

#' Truncated Power Basis Functions
#' 
#' Makes truncated power basis spline functions. 
#' 
#' @param x Vector of values that will be transformed
#' by the basis functions. 
#' @param degree Degree of the polynomial used by the basis 
#' function. 
#' @param nknots Number of knots to use in the spline.  
#' @param knot_loc Location of the knots.  If \code{NULL}
#' they will be placed evenly along the appropriate quantiles 
#' of the variable. 
#' 
#' @export
#' 
#' @return A n x \code{degree}+\code{nknots} matrix of basis 
#' function values. 
tpb <- function(x, degree=3, nknots=3, knot_loc=NULL){
  out <- sapply(1:degree, function(d)x^d)
  if(is.null(knot_loc) ){
    q <- seq(0,1, length=nknots+2)
    q <- q[-c(1, length(q))]  
    s <- quantile(x, q, na.rm=TRUE)
    if(length(s)!=length(unique(s))){
      stop("The quantiles of the variable are not unique.\n")
    }
  }else{
    s <- knot_loc
  }
  for(i in 1:length(s)){
    out <- cbind(out, (x-s[i])^3*(x >= s[i]))
  }
  colnames(out) <- paste0("tpb", 1:ncol(out))
  return(out)
}

#' Shuffle coefficients and standard errors together
#' 
#' Function shuffles together coefficients and standard errors with a significance flag. 
#' 
#' @param b Vector of coefficients
#' @param pv Vector of p-values corresponding to \code{b}
#' @param se Vector of standard errors corresponding to \code{b}
#' @param alpha Alpha level for the significance flag
#' @param digits Number of digits to print
#' @param names A character vector of coefficient names as long as \code{b}
#' 
#' @return A character vector of printed output
#' 
#' @export
#' 
shuffle <- function(b, pv, se, alpha=.05, digits=3, names=NULL){
sig_param <- ifelse(pv < alpha, "*", " ")
coefs <- sprintf(paste0("%.", digits, "f%s"), b, sig_param)
ses <- sprintf(paste0("(%.", digits, "f)"), se)
out <- NULL
for(i in 1:length(coefs)){
  out <- c(out, coefs[i], ses[i])
}
out <- matrix(out, ncol=1)
if(!is.null(names)){
  odds <- seq(1, nrow(out), by=2)
  rownames(out) <- ""
  rownames(out)[odds] <- names
}
out
}
