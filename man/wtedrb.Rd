\name{wtedrb}
\alias{wtedrb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wtedrb(x, y, wts = diag(rep(1, length(y))), scores = wscores)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{wts}{
%%     ~~Describe \code{wts} here~~
}
  \item{scores}{
%%     ~~Describe \code{scores} here~~
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
function (x, y, wts = diag(rep(1, length(y))), scores = wscores) 
{
    ystar <- wts \%*\% y
    xstar <- wts \%*\% x
    first <- rfit(ystar ~ xstar, scores = scores)
    yhstar <- fitted.values(first)
    qrdc <- qr(xstar)
    yhatst <- qr.fitted(qrdc, yhstar, qrdc$rank)
    ehatst <- ystar - yhatst
    bstar <- qr.coef(qrdc, yhatst)
    xps <- solve(t(xstar) \%*\% xstar)
    n <- length(y)
    ones <- matrix(rep(1, n), ncol = 1)
    p1 <- ones \%*\% t(ones)/n
    xc <- xstar - p1 \%*\% xstar
    pc <- xc \%*\% solve(t(xc) \%*\% xc) \%*\% t(xc)
    part1 <- (first$taushat^2) * xps \%*\% t(xstar) \%*\% p1 \%*\% 
        xstar \%*\% xps
    part2 <- (first$tauhat^2) * xps \%*\% t(xstar) \%*\% pc \%*\% xstar \%*\% 
        xps
    vc <- part1 + part2
    se <- sqrt(diag(vc))
    list(yhatst = yhatst, ehatst = ehatst, bstar = bstar, se = se, 
        vc = vc)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
