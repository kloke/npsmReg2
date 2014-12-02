\name{summary.Rnl}
\alias{summary.Rnl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summarize Rank-Based Nonlinear Model Fits }
%%  ~~function to do ... ~~

\description{
Provides a summary similar to the rank-based lonear model fit.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{summary}{wilnl}(object,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class 'wilnl', usually, a result of a call to 'wilnl'}
\item{\dots}{ additional arguments }
%%     ~~Describe \code{object} here~~

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
\author{John Kloke \email{kloke@biostat.wisc.edu}}
%%  ~~who you are~~

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- c(4.4, 4.3 ,1.4, 2.2 ,3.1, 4.2,   2, 3.1 ,  4,  1.9,  2.9,  1.4,  3.2 , 2.5,
   4.4 , 3.6 , 4.3 , 1.5  ,  1 , 4.1 , 1.1 , 1.9 ,   4,  1.1 , 2.2)

y <- c( 8.4 ,8.4 ,1.8, 3.2 ,4.3, 7.6, 3.3, 4.8, 7.5 , 2.4 ,   4 , 1.9 ,   6,  3.6,
   8.5 , 5.3 , 8.8 , 1.4 , 1.8 , 7.1 , 1.4  ,2.9 , 7.3  ,2.6,  2.3)

expmod <- function(x,theta){ exp(x%*%theta) }
expjake <- function(x,theta){ x*exp(x%*%theta) }
fitwil <- wilnl(x,y,0.5,expmod,expjake)
summary(fitwil)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, ...) 
{
    coef <- cbind(object$coef, object$se, object$coef/object$se)
    res <- list(coefficients = coef, it = object$it, call = object$call)
    class(res) <- "summary.Rnl"
    res
  }
}
