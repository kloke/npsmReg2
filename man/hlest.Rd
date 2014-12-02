\name{hlest}
\alias{hlest}
\title{One Sample Hodges Lehmann Estimate }
\description{
Computes the robust one-sample Hodges-Lehmann estimator; i.e., the median of the Walsh averages.
Discussed in most nonparametric books; see, for example, Section 2.3 of Kloke and McKean (2014).
}
\usage{
hlest(x)
}
\arguments{
  \item{x}{ the vector containing the sample.  }
}
\value{
the estimate
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
}
\author{John Kloke and Joe McKean  mckean@wmich.edu }

\examples{
x <- rt(30,2)
hlest(x)
}
