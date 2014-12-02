\name{wilnl}
\alias{wilnl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computes the Rank-Based Fit of a Nonlinear Model
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~

Computes the rank-based fit of a nonlinear model,  either the Wilcoxon (``WIL'') or HBR fit.
The Wilcoxon fit is discussed in Chapter 3 of Hettmansperger and McKean (2011)
and the HBR fit is developed in Abebe and McKean (2014).
See Section 7.7 of Kloke and McKean (2014) for a discussion of the Rfit version.
}
\usage{
wilnl(x, y, theta0, fmodel, jmodel, numstp = 50, eps = 0.001, wts.type = "WIL", intest = "HL", intercept = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{matrix of predictors
}
  \item{y}{response vector
}
  \item{theta0}{initial estimate of nonlinear parameters
}
  \item{fmodel}{R function for the model
}
  \item{jmodel}{R function for the Jacobian
}
  \item{numstp}{maximum number of iterative steps (default is 50)
}
  \item{eps}{precision tolerance (default is 0.001)
}
  \item{wts.type}{either "WIL" (default) for the Wilcoxon fit or "HBR" for the HBR fit
}
  \item{intest}{either "HL" (default) for Hodges-Lehmann estimator of the intercept or "MED" for
the median estimator
}
  \item{intercept}{TRUE if an intercept is in the model else FALSE
}
}
\details{
}
\value{
}
\references{
Abebe, A. and McKean, J.W. (2014),
      Weighted Wilcoxon estimators in nonlinear regression,
      \emph{Australian and New Zealand Journal of Statistics},
      55, 401-420.

Hettmansperger, T.P. and McKean J.W. (2011), \emph{Robust Nonparametric Statistical Methods, 2nd ed.}, New York: Chapman-Hall.

Kloke, J. and McKean, J.W. (2014), \emph{Nonparametric statistical methods using R}, Boca Raton, FL: Chapman-Hall.


}
\author{Joe McKean mckean@wmich.edu and John Kloke kloke@biostat.wisc.edu.
}
\note{
}
\seealso{
}
\examples{
x <- c(4.4, 4.3 ,1.4, 2.2 ,3.1, 4.2,   2, 3.1 ,  4,  1.9,  2.9,  1.4,  3.2 , 2.5,
   4.4 , 3.6 , 4.3 , 1.5  ,  1 , 4.1 , 1.1 , 1.9 ,   4,  1.1 , 2.2)
 
y <- c( 8.4 ,8.4 ,1.8, 3.2 ,4.3, 7.6, 3.3, 4.8, 7.5 , 2.4 ,   4 , 1.9 ,   6,  3.6,
   8.5 , 5.3 , 8.8 , 1.4 , 1.8 , 7.1 , 1.4  ,2.9 , 7.3  ,2.6,  2.3)

expmod <- function(x,theta){ exp(x%*%theta) }
expjake <- function(x,theta){ x*exp(x%*%theta) }
fitwil <- wilnl(x,y,0.5,expmod,expjake)

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

