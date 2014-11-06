wilnl <-
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
    xpxi = solve(t(f1jake) %*% f1jake)
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
                h1 = ones %*% t(ones)/n
                x1 = f1jake - h1 %*% f1jake
                hx1 = x1 %*% solve(t(x1) %*% x1) %*% t(x1)
                ftemp = rfit(resid ~ f1jake)
                varcov = vcov(ftemp, detail = T)
                tau1 = summary.wwfit(ftemp)$tmp2$tau1
                tau = summary.wwfit(ftemp)$tmp2$tau
                part1 = tau1^2 * xpxi %*% t(f1jake) %*% h1 %*% 
                  f1jake %*% xpxi
                part2 = tau^2 * xpxi %*% t(f1jake) %*% hx1 %*% 
                  f1jake %*% xpxi
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
                varcov = xpxi %*% t(f1jake) %*% x1 %*% varcov %*% 
                  t(x1) %*% f1jake %*% xpxi
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
                varcov = xpxi %*% t(f1jake) %*% x1 %*% varcov %*% 
                  t(x1) %*% f1jake %*% xpxi
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
