wtedrb <-
function (x, y, wts = diag(rep(1, length(y))), scores = wscores) 
{
    ystar <- wts %*% y
    xstar <- wts %*% x
    first <- rfit(ystar ~ xstar, scores = scores)
    yhstar <- fitted.values(first)
    qrdc <- qr(xstar)
    yhatst <- qr.fitted(qrdc, yhstar, qrdc$rank)
    ehatst <- ystar - yhatst
    bstar <- qr.coef(qrdc, yhatst)
    xps <- solve(t(xstar) %*% xstar)
    n <- length(y)
    ones <- matrix(rep(1, n), ncol = 1)
    p1 <- ones %*% t(ones)/n
    xc <- xstar - p1 %*% xstar
    pc <- xc %*% solve(t(xc) %*% xc) %*% t(xc)
    part1 <- (first$taushat^2) * xps %*% t(xstar) %*% p1 %*% 
        xstar %*% xps
    part2 <- (first$tauhat^2) * xps %*% t(xstar) %*% pc %*% xstar %*% 
        xps
    vc <- part1 + part2
    se <- sqrt(diag(vc))
    list(yhatst = yhatst, ehatst = ehatst, bstar = bstar, se = se, 
        vc = vc)
}
