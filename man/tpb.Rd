% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effective_functions.r
\name{tpb}
\alias{tpb}
\title{Truncated Power Basis Functions}
\usage{
tpb(x, degree = 3, nknots = 3, knot_loc = NULL)
}
\arguments{
\item{x}{Vector of values that will be transformed
by the basis functions.}

\item{degree}{Degree of the polynomial used by the basis 
function.}

\item{nknots}{Number of knots to use in the spline.}

\item{knot_loc}{Location of the knots.  If \code{NULL}
they will be placed evenly along the appropriate quantiles 
of the variable.}
}
\value{
A n x \code{degree}+\code{nknots} matrix of basis 
function values.
}
\description{
Makes truncated power basis spline functions.
}
\examples{

library(psre)
data(wvs)
smod3 <- lm(secpay ~ tpb(gini_disp, degree=3, knot_loc=.35) + democrat, data=wvs)
summary(smod3)

}
