\name{wilnl}
\alias{wilnl}
\title{Computes the Rank-Based Fit of a Nonlinear Model }
\description{
Computes the rank-based fit of a nonlinear model,  either the Wilcoxon (``WIL'') or HBR fit.
The Wilcoxon fit is discussed in Chapter 3 of Hettmansperger and McKean (2011)
and the HBR fit is developed in Abebe and McKean (2014).
See Section 7.7 of Kloke and McKean (2014) for a discussion of the Rfit version.
}
\usage{
wilnl(x, y, theta0, fmodel, jmodel, numstp = 50, eps = 0.001, wts.type = "WIL", intest = "HL", intercept = FALSE)
}
\arguments{
  \item{x}{matrix of predictors }
  \item{y}{response vector }
  \item{theta0}{initial estimate of nonlinear parameters }
  \item{fmodel}{R function for the model }
  \item{jmodel}{R function for the Jacobian }
  \item{numstp}{maximum number of iterative steps (default is 50) }
  \item{eps}{precision tolerance (default is 0.001) }
  \item{wts.type}{either "WIL" (default) for the Wilcoxon fit or "HBR" for the HBR fit }
  \item{intest}{either "HL" (default) for Hodges-Lehmann estimator of the intercept or "MED" for the median estimator }
  \item{intercept}{TRUE if an intercept is in the model else FALSE }
}
\references{
Abebe, A. and McKean, J.W. (2014),
      Weighted Wilcoxon estimators in nonlinear regression,
      \emph{Australian and New Zealand Journal of Statistics},
      55, 401-420.

Hettmansperger, T.P. and McKean J.W. (2011), \emph{Robust Nonparametric Statistical Methods, 2nd ed.}, New York: Chapman-Hall.

Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.

}
\author{Joe McKean mckean@wmich.edu and John Kloke kloke@biostat.wisc.edu}
