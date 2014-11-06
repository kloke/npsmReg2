print.summary.Rnl <-
function (x, digits = max(5, .Options$digits - 2), ...) 
{
    cat("Call:\n")
    print(x$call)
    cat("\nCoefficients:\n")
    coef <- x$coefficients
    colnames(coef) <- c("Estimate", "Std. Error", "t.value")
    printCoefmat(coef, P.values = FALSE, has.Pvalue = FALSE)
    cat("\nNumber of iterations: ", x$it, "\n")
}
