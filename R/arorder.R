arorder <-
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
        tst <- t(amat %*% est) %*% solve(amat %*% varcov %*% 
            t(amat)) %*% amat %*% est/q
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
