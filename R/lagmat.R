lagmat <-
function (x, p) 
{
    n <- length(x)
    xmat <- matrix(ncol = p, nrow = n - p)
    resp <- x[(p + 1):n]
    for (j in 1:p) {
        xmat[, j] <- x[(p - j + 1):(n - j)]
    }
    lagmat <- cbind(resp, xmat)
}
