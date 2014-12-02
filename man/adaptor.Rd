\name{adaptor}
\alias{adaptor}
\title{Hogg-Type Adaptive Scheme for Linear Models }
\description{
Function that computes a Hogg-type adaptive scheme for linear models.
This scheme is dicussed in detail in Section 7.6 of kloke and McKean (2014).
}
\usage{
adaptor(xmat, y, delta = 0.8, hparm = 2)
}
\arguments{
  \item{xmat}{design matrix }
  \item{y}{vector of responses }
  \item{delta}{window type parameter used in estimate of scale parameter tau; default is 0.80.
}
  \item{hparm}{Huber's cutoff value for outliers used in estimate of scale parameter tau; default is 2.
}
}
\value{
      \item{fitwil}{Rank-based fit of linear model using Wilcoxon scores}
      \item{fitsc}{Rank-based fit of linear model using score function selected by adaptive procedure}
      \item{fitsc}{Score selected by adaptive procedure.}
      \item{iscore}{Numerical indicator of score selected by adaptive procedure.}
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
}
\author{ Joe McKean, mckean@wmich.edu }

\examples{
y <- rnorm(40)
x <- matrix(rnorm(120),ncol=3)
adaptor(x,y)
}
