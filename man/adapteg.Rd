\name{adapteg}
\alias{adapteg}
\docType{data}
\title{
Generated Adaptive Regression Data Set
}
\description{
This is a generated data set which is discussed in Example 7.6.1 on page 199 of Kloke and McKean (2014).
It illustrates the Hogg-type adaptive scheme for regression discussed in Section 7.6 of this book.
The two predictors are generated standard normal variates while the errors are generated exponential
variates.
All true regression parameters were set to 0.
}
\usage{data("adapteg")}
\format{
Forty observations on three variables.
\describe{
    \item{\code{x1}}{first predictor}
    \item{\code{x2}}{second predictor}
    \item{\code{y}}{response vector}
  }
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
}
\examples{
data(adapteg)
xmat <- cbind(adapteg[,1],adapteg[,2])
y<-adapteg[,3]
adapt <- adaptor(xmat,y)
summary(adapt$fitsc)
adapt$iscore
}
\keyword{datasets}
