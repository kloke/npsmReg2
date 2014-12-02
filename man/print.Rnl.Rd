\name{print.Rnl}
\alias{print.Rnl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Internal Print Function for wilnl
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Internal Print Function for the Rank-Based Nonlinear Fit Function wilnl.
These functions print the output in a user-friendly manner using the internal R function \code{print}.


}
\usage{
\method{print.Rnl}{wilnl}(x, ...)
\method{print}{summary.wilnl}(x, digits = max(5, .Options$digits - 2), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object to be printed }
%%     ~~Describe \code{x} here~~
  \item{digits}{number of digits to display}
  \item{\dots}{  additional arguments to be passed to \code{print} }

%%     ~~Describe \code{\dots} here~~

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{John Kloke kloke@biostat.wisc.edu
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, ...) 
{
    cat("Call:\n")
    print(x$call)
    cat("\nCoefficients:\n")
    print(x$coef, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
