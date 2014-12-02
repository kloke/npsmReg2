\name{print.Rnl}
\alias{print.Rnl}
\title{Internal Print Function for wilnl }
\description{
Internal Print Function for the Rank-Based Nonlinear Fit Function wilnl.
These functions print the output in a user-friendly manner using the internal R function \code{print}.
}
\usage{
\method{print.Rnl}{wilnl}(x, ...)
\method{print}{summary.wilnl}(x, digits = max(5, .Options$digits - 2), ...)
}
\arguments{
  \item{x}{An object to be printed }
  \item{digits}{number of digits to display}
  \item{\dots}{  additional arguments to be passed to \code{print} }
}
\author{John Kloke kloke@biostat.wisc.edu }
