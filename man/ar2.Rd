\name{ar2}
\alias{ar2}
\docType{data}
\title{ Generated AR(2) Data Set }
\description{
This time series data was genearated by the R command  stream=genarp(20,50,.6,-.3,0).
It is used in several examples in Section 7.8 of Kloke and McKean (2014).
}
\usage{data("ar2")}
\format{
Fifty observations on one variable.
\describe{
    \item{\code{ar2}}{the time series}
  }

}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
}
\examples{
data(ar2)
}
