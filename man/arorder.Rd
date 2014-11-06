\name{arorder}
\alias{arorder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
arorder(n, maxp, est, varcov, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{maxp}{
%%     ~~Describe \code{maxp} here~~
}
  \item{est}{
%%     ~~Describe \code{est} here~~
}
  \item{varcov}{
%%     ~~Describe \code{varcov} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
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
function (n, maxp, est, varcov, alpha = 0.05) 
{
    p <- maxp
    amat <- matrix(rep(0, p + 1), ncol = p + 1)
    zmat <- rep(0, p + 1)
    pl <- p + 1
    amat[pl] <- 1
    ic <- 0
    res <- rep(0, 3)
    zed <- c(0)
    while (pl > 1) {
        q <- length(amat[, 1])
        tst <- t(amat \%*\% est) \%*\% solve(amat \%*\% varcov \%*\% 
            t(amat)) \%*\% amat \%*\% est/q
        pval <- 1 - pf(tst, q, n - maxp - 1)
        res <- rbind(res, c(p, tst, pval))
        if (pval <= alpha) {
            ord <- p
            pl <- 1
        }
        else {
            pl <- pl - 1
            p <- p - 1
            tmp <- zmat
            tmp[pl] <- 1
            amat <- rbind(amat, tmp)
            zed <- c(zed, 0)
        }
    }
    if (p == 0) {
        ord <- "NA"
    }
    res <- res[2:length(res[, 1]), ]
    list(orderofar = ord, results = res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
