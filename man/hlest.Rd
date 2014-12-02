\name{hlest}
\alias{hlest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{One Sample Hodges Lehmann Estimate
%%  ~~function to do ... ~~
}
\description{
Computes the robust one-sample Hodges-Lehmann estimator; i.e., the median of the Walsh averages.
Discussed in most nonparametric books; see, for example, Section 2.3 of Kloke and McKean (2014).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hlest(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
the vector containing the sample.
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the estimator
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.

%% ~put references to the literature/web site here ~
}
\author{John Kloke and Joe McKean  mckean@wmich.edu
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
x <- rt(30,2)
hlest(x)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
    xpairs = pairup(x, type = "leq")
    was = (xpairs[, 1] + xpairs[, 2])/2
    hlest = median(was)
    hlest
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
