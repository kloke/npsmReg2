\name{wtedrb}
\alias{wtedrb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted Rank-Based Fit
%%  ~~function to do ... ~~
}
\description{
This function computes the weighted rank-based fit.   Input are the responses, design matrix, weights
(diagonal or matrix, assumes postive definite), and scores.
See Section 7.4 of Kloke and McKean (2014).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wtedrb(x, y, wts = diag(rep(1, length(y))), scores = wscores)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{design matrix}
  \item{y}{vector of responses}
  \item{wts}{either a matrix of weights or a vector of diagonal weights (assume weights are positive definite).}
  \item{scores}{rank-based scores to use, (default is Wilcoxon)}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{yhatst }{vector of weighted fitted values}
  \item{ehatst }{residuals of weighted fit}
  \item{bstar }{vector of weighted regression coefficients}
  \item{se }{vector of standard errors of bstar}
  \item{vc }{variance-covariance of bstar}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall
%% ~put references to the literature/web site here ~
}
\author{Joe McKean (mckean@wmich.edu) and John Kloke (kloke@biostat.wisc.edu)
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
## Example 7.4.1 in Kloke and McKean (2014)
x <- seq(2,28,2)
y <- c(.08, 1.12,4.43,4.98,4.92,7.18,5.57,8.40,8.881,10.81,11.16,10.12,13.12,15.04)
wtedfit <- wtedrb(x,y)
wtedfit$bstar

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
