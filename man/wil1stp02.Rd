\name{wil1stp02}
\alias{wil1stp02}
\title{ Rank-Based Nonlinear Subroutine }
\description{
Subroutine that computes the rank-based fit for each iterative
step in the rank-based nonlinear fit function wilnl.R.
}
\usage{
wil1stp02(x, y, theta0, fmodel, jmodel, wts.type = "WIL", intest = "HL", intercept = FALSE)
}
\arguments{
  \item{x}{matrix of predictors }
  \item{y}{response vector }
  \item{theta0}{initial estimate of parameters used at the beginning of the step }
  \item{fmodel}{the function that computes the nonlinear model }
  \item{jmodel}{the function that computes the jacobian }
  \item{wts.type}{either ``WIL'' (default) for the Wilcoxon fit of ``HBR'' for the HBR fit.  } 
  \item{intest}{for the estimate of the intercept:  ``HL'' (default) computes the Hodges-Lehmann estimate based on the residuals and  ``MED'' computes  the median of the residuals.  }
  \item{intercept}{TRUE if an intercept is in the nonlinear model else FALSE (default).  }
}
\value{
  \item{theta1 }{new estimate of nonlinear parameters}
  \item{delstar }{theta1 minus theta0}
  \item{resid0 }{residual at end of step}
  \item{sse0 }{sum of squared errors for initial fit}
  \item{sse1 }{sum of squared errors for final (at end of the step) fit}
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
}
\author{Joe McKean (mckean@wmich.edu) and John Kloke (kloke@biostat.wisc.edu) }
