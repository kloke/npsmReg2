\name{lagmat}
\alias{lagmat}
\title{Creates Lagged cwVariablesMatrix for AR Models }
\description{
Given the time series and the order p of the AR series, this function returns the lagged variables of order p.
The lagged response is in the first column followed by the lagged x's in increasing order.
}
\usage{
lagmat(x, p)
}
\arguments{
  \item{x}{time series}
  \item{p}{ order of the AR series }
}
\value{
  \item{xmat }{a matrix with p+1 columns, the first column is the lagged response, the remaining columns
are the lagged x's in increasing order.}
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
}
\author{
Joe McKean mckean@wmich.edu and John Kloke kloke@biostat.wisc.edu
}
\examples{
x <- rnorm(40)
xlags <- lagmat(x,4)
}
