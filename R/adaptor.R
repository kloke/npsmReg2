adaptor <-
function (xmat, y, delta = 0.8, hparm = 2) 
{
    n <- length(y)
    clq1 <- 0.36 + (0.68/n)
    cuq1 <- 2.73 - (3.72/n)
    if (n < 25) {
        clq2 <- 2.17 - (3.01/n)
        cuq2 <- 2.63 - (3.94/n)
    }
    else {
        clq2 <- 2.24 - (4.68/n)
        cuq2 <- 2.95 - (9.37/n)
    }
    fitw <- rfit(y ~ xmat, delta = delta, hparm = hparm)
    ehat <- residuals(fitw)
    q1 <- Q1(ehat)
    q2 <- Q2(ehat)
    if (q1 <= clq1) {
        if (q2 <= clq2) {
            iscore <- 1
            sc <- bentscores2
            sc@param = c(0.15, 0.65, -1, 2, 0)
        }
        if ((q2 > clq2) && (q2 <= cuq2)) {
            iscore <- 2
            sc <- bentscores3
            sc@param = c(0.3, -1, 2)
        }
        if (q2 > cuq2) {
            iscore <- 3
            sc <- bentscores3
            sc@param = c(0.5, -1, 2)
        }
    }
    if ((q1 > clq1) && (q1 <= cuq1)) {
        if (q2 <= clq2) {
            iscore <- 4
            sc <- bentscores2
            sc@param = c(0.25, 0.75, -1, 1, 0)
        }
        if ((q2 > clq2) && (q2 <= cuq2)) {
            iscore <- 5
            sc <- wscores
        }
        if (q2 > cuq2) {
            iscore <- 6
            sc <- bentscores4
            sc@param = c(0.25, 0.75, -1, 1, 0)
        }
    }
    if (q1 > cuq1) {
        if (q2 <= clq2) {
            iscore <- 7
            sc <- bentscores2
            sc@param = c(0.35, 0.85, -2, 1, 0)
        }
        if ((q2 > clq2) && (q2 <= cuq2)) {
            iscore <- 8
            sc <- bentscores1
            sc@param = c(0.7, -2, 1)
        }
        if (q2 > cuq2) {
            iscore <- 9
            sc <- bentscores1
            sc@param = c(0.5, -2, 1)
        }
    }
    fitsc <- rfit(y ~ xmat, scores = sc, delta = delta, hparm = hparm)
    list(fitwil = fitw, fitsc = fitsc, sc = sc, iscore = iscore)
}
