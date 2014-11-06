\name{wilnl}
\alias{wilnl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wilnl(x, y, theta0, fmodel, jmodel, numstp = 50, eps = 0.001, wts.type = "WIL", intest = "HL", intercept = FALSE)
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
  \item{numstp}{
%%     ~~Describe \code{numstp} here~~
}
  \item{eps}{
%%     ~~Describe \code{eps} here~~
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
function (x, y, theta0, fmodel, jmodel, numstp = 50, eps = 0.001, 
    wts.type = "WIL", intest = "HL", intercept = FALSE) 
{
    x <- as.matrix(x)
    it = 0
    ic = 1
    iend = numstp
    p = length(theta0)
    n = length(y)
    coll = matrix(rep(0, (numstp + 1) * (p + 1)), ncol = (p + 
        1))
    coll[1, 1:p] = theta0
    while (ic <= iend) {
        it = it + 1
        temp = wil1stp02(x, y, theta0, fmodel, jmodel, wts.type, 
            intest, intercept)
        delstar = temp$delstar
        theta1 = temp$theta1
        if (ic == 1) {
            coll[1, (p + 1)] = temp$sse0
        }
        coll[(ic + 1), 1:p] = temp$theta1
        coll[(ic + 1), (p + 1)] = temp$sse1
        conchk = sqrt(sum(delstar^2)/sum(theta0^2))
        if (conchk < eps) {
            ic = iend + 1
        }
        else {
            theta0 = theta1
            ic = ic + 1
        }
    }
    yhat = fmodel(x, theta1)
    resid = y - yhat
    p = length(theta1)
    tauhat = gettau(resid, (p + 1))
    f1jake = jmodel(x, theta1)
    xpxi = solve(t(f1jake) \%*\% f1jake)
    if (wts.type == "WIL") {
        if (intest == "HL") {
            varcov = tauhat^2 * xpxi
        }
        if (intest == "MED") {
            if (intercept) {
                xtemp = f1jake[, 2:p]
                ftemp = rfit(resid ~ xtemp)
                varcov = vcov(ftemp, detail = T)$varcov
            }
            else {
                ones = matrix(rep(1, n), ncol = 1)
                h1 = ones \%*\% t(ones)/n
                x1 = f1jake - h1 \%*\% f1jake
                hx1 = x1 \%*\% solve(t(x1) \%*\% x1) \%*\% t(x1)
                ftemp = rfit(resid ~ f1jake)
                varcov = vcov(ftemp, detail = T)
                tau1 = summary.wwfit(ftemp)$tmp2$tau1
                tau = summary.wwfit(ftemp)$tmp2$tau
                part1 = tau1^2 * xpxi \%*\% t(f1jake) \%*\% h1 \%*\% 
                  f1jake \%*\% xpxi
                part2 = tau^2 * xpxi \%*\% t(f1jake) \%*\% hx1 \%*\% 
                  f1jake \%*\% xpxi
                varcov = part1 + part2
            }
        }
    }
    if (wts.type == "HBR") {
        if (intest == "HL") {
            if (intercept) {
                xtemp = f1jake[, 2:p]
            }
            else {
                xtemp = f1jake
            }
            ftemp = hbrfit(resid ~ xtemp)
            vc = vcov(ftemp)
            tau1 = ftemp$taushat
            tau = ftemp$tauhat
            kappa = (tau^2 - tau1^2)/n
            varcov = vc
            varcov[1, 1] = varcov[1, 1] + kappa
            if (!intercept) {
                x1 = cbind(rep(1, n), f1jake)
                varcov = xpxi \%*\% t(f1jake) \%*\% x1 \%*\% varcov \%*\% 
                  t(x1) \%*\% f1jake \%*\% xpxi
            }
        }
        else {
            if (intercept) {
                xtemp = f1jake[, 2:p]
            }
            else {
                xtemp = f1jake
            }
            ftemp = hbrfit(resid ~ xtemp)
            varcov = vcov(ftemp, detail = T)
            if (!intercept) {
                x1 = cbind(rep(1, n), f1jake)
                varcov = xpxi \%*\% t(f1jake) \%*\% x1 \%*\% varcov \%*\% 
                  t(x1) \%*\% f1jake \%*\% xpxi
            }
        }
    }
    se = sqrt(diag(varcov))
    coll = coll[1:(it + 1), ]
    res <- list(coef = theta1, coll = coll, it = it, tauhat = tauhat, 
        varcov = varcov, se = se, x = x, y = y, fitted.values = yhat, 
        residuals = resid, xjake = f1jake, it = (it + 1))
    res$call <- match.call()
    class(res) <- list("Rnl")
    res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
