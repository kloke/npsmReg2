\name{adaptor}
\alias{adaptor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
adaptor(xmat, y, delta = 0.8, hparm = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xmat}{
%%     ~~Describe \code{xmat} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{delta}{
%%     ~~Describe \code{delta} here~~
}
  \item{hparm}{
%%     ~~Describe \code{hparm} here~~
}
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
\author{
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
function (xmat, y, delta = 0.8, hparm = 2) 
{
    n <- length(y)
    clq1 <- 0.36 + (0.68/n)
    cuq1 <- 2.73 - (3.72/n)
    if (n < 25) {
        clq2 <- 2.17 - (3.01/n)
        cuq2 <- 2.63 - (3.94/n)
    }
    else {
        clq2 <- 2.24 - (4.68/n)
        cuq2 <- 2.95 - (9.37/n)
    }
    fitw <- rfit(y ~ xmat, delta = delta, hparm = hparm)
    ehat <- residuals(fitw)
    q1 <- Q1(ehat)
    q2 <- Q2(ehat)
    if (q1 <= clq1) {
        if (q2 <= clq2) {
            iscore <- 1
            sc <- bentscores2
            sc@param = c(0.15, 0.65, -1, 2, 0)
        }
        if ((q2 > clq2) && (q2 <= cuq2)) {
            iscore <- 2
            sc <- bentscores3
            sc@param = c(0.3, -1, 2)
        }
        if (q2 > cuq2) {
            iscore <- 3
            sc <- bentscores3
            sc@param = c(0.5, -1, 2)
        }
    }
    if ((q1 > clq1) && (q1 <= cuq1)) {
        if (q2 <= clq2) {
            iscore <- 4
            sc <- bentscores2
            sc@param = c(0.25, 0.75, -1, 1, 0)
        }
        if ((q2 > clq2) && (q2 <= cuq2)) {
            iscore <- 5
            sc <- wscores
        }
        if (q2 > cuq2) {
            iscore <- 6
            sc <- bentscores4
            sc@param = c(0.25, 0.75, -1, 1, 0)
        }
    }
    if (q1 > cuq1) {
        if (q2 <= clq2) {
            iscore <- 7
            sc <- bentscores2
            sc@param = c(0.35, 0.85, -2, 1, 0)
        }
        if ((q2 > clq2) && (q2 <= cuq2)) {
            iscore <- 8
            sc <- bentscores1
            sc@param = c(0.7, -2, 1)
        }
        if (q2 > cuq2) {
            iscore <- 9
            sc <- bentscores1
            sc@param = c(0.5, -2, 1)
        }
    }
    fitsc <- rfit(y ~ xmat, scores = sc, delta = delta, hparm = hparm)
    list(fitwil = fitw, fitsc = fitsc, sc = sc, iscore = iscore)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
