\name{diagdata}
\alias{diagdata}
\docType{data}
\title{
Generated Data Set for Robust Diagnostics 
}
\description{
This is the generated data set used for the discussion of robust diagnotics
in Section 7.3.
}
\usage{data(diagdata)}
\format{
Thirty observations on four variables.
\describe{
    \item{\code{x1}}{first predictor}
    \item{\code{x2}}{second predictor}
    \item{\code{ygood}}{good responses}
    \item{\code{ybad}}{bad responses}
  }

}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
}
\examples{
data(diagdata)
hbrfit(ygood~x1+x2,data=diagdata)
hbrfit(ybad~x1+x2,data=diagdata)
dwilhbr = with(diagdata,fitdiag(cbind(x1,x2),ybad,est=c("WIL","HBR")))
bmtdbetas <- dwilhbr$tdbeta
tdbetas <- c(dwilhbr$tdbeta,dwilhbr$bmtd)
cfit <- dwilhbr$cfit
plot(1:30,cfit,xlab="Case",ylab="Change in fits")
title(main="Change Between Fits: Wilcoxon and HBR")
}
\keyword{datasets}
