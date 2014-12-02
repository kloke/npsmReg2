\name{lagmat}
\alias{lagmat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Creates Lagged cwVariablesMatrix for AR Models
%%  ~~function to do ... ~~
}
\description{
Given the time series and the order p of the AR series, this function returns the lagged variables of order p.
The lagged response is in the first column followed by the lagged x's in increasing order.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lagmat(x, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{time series
%%     ~~Describe \code{x} here~~
}
  \item{p}{
order of the AR series
%%     ~~Describe \code{p} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{xmat }{a matrix with p+1 columns, the first column is the lagged response, the remaining columns
are the lagged x's in increasing order.}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.
%% ~put references to the literature/web site here ~
}
\author{
Joe McKean mckean@wmich.edu and John Kloke kloke@biostat.wisc.edu
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- rnorm(40)
xlags <- lagmat(x,4)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, p) 
{
    n <- length(x)
    xmat <- matrix(ncol = p, nrow = n - p)
    resp <- x[(p + 1):n]
    for (j in 1:p) {
        xmat[, j] <- x[(p - j + 1):(n - j)]
    }
    lagmat <- cbind(resp, xmat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
