% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effective_functions.r
\name{lsa}
\alias{lsa}
\title{Linear Scatterplot Array}
\usage{
lsa(
  formula,
  xlabels = NULL,
  ylab = NULL,
  data,
  ptsize = 1,
  ptshape = 1,
  ptcol = "gray65",
  linear = TRUE,
  loess = TRUE,
  lm_args = linear_args(),
  lo_args = loess_args(),
  ptalpha = 1,
  ...
)
}
\arguments{
\item{formula}{Formula giving the variables to be plotted.}

\item{xlabels}{Vector of character strings giving the labs of
variables to be used in place of the variable names.}

\item{ylab}{Character string giving y-variable label to be
used instead of variable name.}

\item{data}{A data frame that holds the variables to be plotted.}

\item{ptsize}{Size of points.}

\item{ptshape}{Shape of points.}

\item{ptcol}{Color of points.}

\item{linear}{Logical indicating whether linear regression line is included.}

\item{loess}{Logical indicating whether loess smooth should be included.}

\item{lm_args}{A list of arguments passed to `geom_smooth()` for the linear regression line.}

\item{lo_args}{A list or arguments passed to `geom_smooth()` for the loess smooth.}

\item{ptalpha}{Alpha of points.}

\item{...}{Other arguments passed down, currently not implemented.}
}
\value{
A \code{cowplot} object.
}
\description{
Produces a linear scatterplot array with marginal histograms
}
\examples{
data(wvs)
lsa(formula = as.formula(sacsecval ~ resemaval + moral + 
                           pct_univ_degree + pct_female + 
                           pct_low_income), 
  xlabels = c("Emancipative Vals", "Moral Perm", 
              "\% Univ Degree", "\% Female", "\% Low Income"), 
  ylab = "Secular Values", 
  data=wvs)
}
