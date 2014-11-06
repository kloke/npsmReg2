\name{wil1stp02}
\alias{wil1stp02}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wil1stp02(x, y, theta0, fmodel, jmodel, wts.type = "WIL", intest = "HL", intercept = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{theta0}{
%%     ~~Describe \code{theta0} here~~
}
  \item{fmodel}{
%%     ~~Describe \code{fmodel} here~~
}
  \item{jmodel}{
%%     ~~Describe \code{jmodel} here~~
}
  \item{wts.type}{
%%     ~~Describe \code{wts.type} here~~
}
  \item{intest}{
%%     ~~Describe \code{intest} here~~
}
  \item{intercept}{
%%     ~~Describe \code{intercept} here~~
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
function (x, y, theta0, fmodel, jmodel, wts.type = "WIL", intest = "HL", 
    intercept = FALSE) 
{
    xmat = jmodel(x, theta0)
    yhat0 = fmodel(x, theta0)
    z = y - yhat0
    sse0 = sum(z^2)
    if (intercept) {
        kold = length(xmat[1, ])
        xmathold = xmat
        xmat = xmat[, 2:kold]
    }
    if (wts.type == "WIL") {
        see5 = rfit(z ~ xmat)
    }
    if (wts.type == "HBR") {
        see5 = hbrfit(z ~ xmat)
    }
    resid0 = see5$resid
    if (intest == "HL") {
        tresid = see5$resid + see5$coef[1]
        hl = hlest(tresid)
        resid0 = tresid - hl
    }
    if (intest == "MED") {
        resid0 = see5$resid
    }
    if (intercept) {
        xmat = xmathold
    }
    yhat1 = z - resid0
    delstar = solve(t(xmat) \%*\% xmat) \%*\% t(xmat) \%*\% yhat1
    theta1 = delstar + theta0
    yhat1 = fmodel(x, theta1)
    z1 = y - yhat1
    sse1 = sum(z1^2)
    list(theta1 = theta1, delstar = delstar, resid0 = resid0, 
        sse0 = sse0, sse1 = sse1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
