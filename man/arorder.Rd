\name{arorder}
\alias{arorder}
\title{ Algorithm for Order of an Autoregressive Time Series }
\description{
This is an R implementation of the order of an autoregressive series proposed
in Terpstra et al. (2001).   The R function is discussed in detail in Section 7.8.1
of Kloke and McKean (2014).   The sequential testing is a Wald-tyoe of test.
}
\usage{ arorder(n, maxp, est, varcov, alpha = 0.05) }
\arguments{
  \item{n}{length of series}
  \item{maxp}{maximal order}
  \item{est}{estimates of time series coefficients for AR(maxp)}
  \item{varcov}{variance-covariance matrix of estimates}
  \item{alpha}{significance level, (default is 0.05)}
}
\value{
     \item{orderofar}{order selected by algorithm}
     \item{results}{matrix with stepwise results from algorithm}
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.

Terpstra, J, McKean, J.W., and Naranjo, J.D. (2001), GR-estimates for an autoregressive time series,
\emph{Statistics and Probability Letters}, 51, 165-172.

}
\author{ Joe McKean mckean@wmich.edu }

\examples{
data(ar2)
data <- lagmat(ar2,4)
x <- data[,1]
xmat <- data[,2:(4+1)]
hbrv<- hbrfit(x~xmat)
varcov <- vcov(hbrv,details=TRUE)
est<-hbrv$coef
alg<-arorder(length(x),4,est,varcov)
}
