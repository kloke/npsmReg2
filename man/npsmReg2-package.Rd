\name{npsmReg2-package}
\alias{npsmReg2-package}
\alias{npsmReg2}
\docType{package}
\title{
Chapter 7 Regression II, Kloke and McKean (2014)
~~ package title ~~
}
\description{
Package provides functions discussed in Chapter 7 Regression II, Kloke and McKean (2014).
This includes HBR estimates, robust diagnostics, weighted regression, a Hogg-type adaptive procedure,
nonlinear rank-based fits, and rank-based time series (autoregressive).
}
\details{
\tabular{ll}{
Package: \tab npsmReg2\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-10-12\cr
License: \tab What license is it under?\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
John Kloke, Joesph McKean

Maintainer: John Kloke <kloke@biostat.wisc.edu>


}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.

Hettmansperger, T.P. and McKean J.W. (2011), \emph{Robust Nonparametric Statistical Methods, 2nd ed.}, New York: Chapman-Hall.

~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
#   nonlinear rnak-based fit
x <- c(4.4, 4.3 ,1.4, 2.2 ,3.1, 4.2,   2, 3.1 ,  4,  1.9,  2.9,  1.4,  3.2 , 2.5,
   4.4 , 3.6 , 4.3 , 1.5  ,  1 , 4.1 , 1.1 , 1.9 ,   4,  1.1 , 2.2)

y <- c( 8.4 ,8.4 ,1.8, 3.2 ,4.3, 7.6, 3.3, 4.8, 7.5 , 2.4 ,   4 , 1.9 ,   6,  3.6,
   8.5 , 5.3 , 8.8 , 1.4 , 1.8 , 7.1 , 1.4  ,2.9 , 7.3  ,2.6,  2.3)

expmod <- function(x,theta){ exp(x%*%theta) }
expjake <- function(x,theta){ x*exp(x%*%theta) }
fitwil <- wilnl(x,y,0.5,expmod,expjake)
summary(fitwil)

## Example 7.4.1 in Kloke and McKean (2014) of a weighted rank-based fit.
x <- seq(2,28,2)
y <- c(.08, 1.12,4.43,4.98,4.92,7.18,5.57,8.40,8.881,10.81,11.16,10.12,13.12,15.04)
wtedfit <- wtedrb(x,y)
wtedfit$bstar

#  Hogg-type adative estimator for a linear model
y <- rnorm(40)
x <- matrix(rnorm(120),ncol=3)
adaptor(x,y)

#   Determination of the order of an autoregressive time series
load('ar2.rda')
data <- lagmat(ar2,4)
x <- data[,1]
xmat <- data[,2:(4+1)]
hbrv<- hbrfit(x~xmat)
varcov <- vcov(hbrv,details=T)
est<-hbrv$coef
alg<-arorder(length(x),4,est,varcov)


~~ simple examples of the most important functions ~~
}
