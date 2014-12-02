\name{wtedrb}
\alias{wtedrb}
\title{ Weighted Rank-Based Fit }
\description{
This function computes the weighted rank-based fit.   Input are the responses, design matrix, weights
(diagonal or matrix, assumes postive definite), and scores.
See Section 7.4 of Kloke and McKean (2014).
}
\usage{
wtedrb(x, y, wts = diag(rep(1, length(y))), scores = wscores)
}
\arguments{
  \item{x}{design matrix}
  \item{y}{vector of responses}
  \item{wts}{either a matrix of weights or a vector of diagonal weights (assume weights are positive definite).}
  \item{scores}{rank-based scores to use, (default is Wilcoxon)}
}
\value{
  \item{yhatst }{vector of weighted fitted values}
  \item{ehatst }{residuals of weighted fit}
  \item{bstar }{vector of weighted regression coefficients}
  \item{se }{vector of standard errors of bstar}
  \item{vc }{variance-covariance of bstar}
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall
}
\author{Joe McKean (mckean@wmich.edu) and John Kloke (kloke@biostat.wisc.edu)
}

\examples{
## Example 7.4.1 in Kloke and McKean (2014)
x <- seq(2,28,2)
y <- c(.08, 1.12,4.43,4.98,4.92,7.18,5.57,8.40,8.881,10.81,11.16,10.12,13.12,15.04)
wtedfit <- wtedrb(x,y)
wtedfit$bstar
}
