\name{are2}
\alias{are2}
\docType{data}
\title{
Generated AR(2) Data Set
%%   ~~ data name/kind ... ~~
}
\description{
This time series data was genearated by the R command  stream=genarp(20,50,.6,-.3,0).
It is used in several examples in Section 7.8 of Kloke and McKean (2014).
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("are2")}
\format{
Fifty observations on one variable.
\describe{
    \item{\code{ar2}}{the time series}
  }

}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.

%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(ar2)
}
\keyword{datasets}
