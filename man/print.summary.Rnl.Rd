\name{print.summary.Rnl}
\alias{print.summary.Rnl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
wilnl Internal Print Functions
%%  ~~function to do ... ~~
}
\description{
These functions print the output in a user-friendly manner using the internal R function \code{print}.


%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.summary.Rnl(x, digits = max(5, .Options$digits - 2), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object to be printed }
%%     ~~Describe \code{x} here~~
  \item{digits}{number of digits to display}
  \item{\dots}{  additional arguments to be passed to \code{print} }

%%     ~~Describe \code{digits} here~~
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
\author{John Kloke \email{kloke@biostat.wisc.edu}
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
function (x, digits = max(5, .Options$digits - 2), ...) 
{
    cat("Call:\n")
    print(x$call)
    cat("\nCoefficients:\n")
    coef <- x$coefficients
    colnames(coef) <- c("Estimate", "Std. Error", "t.value")
    printCoefmat(coef, P.values = FALSE, has.Pvalue = FALSE)
    cat("\nNumber of iterations: ", x$it, "\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
