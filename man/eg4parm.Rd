\name{eg4parm}
\alias{eg4parm}
\docType{data}
\title{
Generated 4 Parameter Logistic Data
}
\description{
This is the generated data set from a nonlinear 4-parameter logistic model
which is discussed in Example 7.7.3, page 210, of Kloke and McKean (2014).
}
\usage{data("eg4parm")}
\format{
Twenty-four observations on three variables.
\describe{
    \item{\code{x}}{predictor}
    \item{\code{y1}}{original response}
    \item{\code{y2}}{changed response}
  }

}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
}
\examples{
data(eg4parm)
data <- eg4parm

func <- function(z,theta){
     d = theta[1]; s = theta[2]; c = theta[3]; b = theta[4]
     func <- (s/(1 + exp(b*(z*log(10) - log(c))))) + d
     func
}
jake = function(z,theta){
     d = theta[1]; s = theta[2]; c = theta[3]; b = theta[4]
     xp = 1 + exp(b*(z*log(10) - log(c)))
     fd = 1; fs = 1/xp
     fc = s*(xp^(-2))*((b/c)*exp(b*(z*log(10) - log(c))))
     fb = -s*(xp^(-2))*((z*log(10) - log(c))*exp(b*(z*log(10) - log(c))))
     jake = cbind(fd,fs,fc,fb); jake
}

a <- 10; b <- -1.2; c <- 3; d <- 110; s <- -100;
z <- log10(data[,1]); z <- matrix(z,ncol=1); y<-data[,2]
theta0 = c(d,s,c,b); # theta0<-matrix(theta0,ncol=1)

fitwil = wilnl(z,y, theta0,func,jake,intercept=TRUE)
summary(fitwil)

}
\keyword{datasets}
