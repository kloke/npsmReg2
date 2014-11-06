wil1stp02 <-
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
    delstar = solve(t(xmat) %*% xmat) %*% t(xmat) %*% yhat1
    theta1 = delstar + theta0
    yhat1 = fmodel(x, theta1)
    z1 = y - yhat1
    sse1 = sum(z1^2)
    list(theta1 = theta1, delstar = delstar, resid0 = resid0, 
        sse0 = sse0, sse1 = sse1)
}
