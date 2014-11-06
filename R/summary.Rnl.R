summary.Rnl <-
function (object, ...) 
{
    coef <- cbind(object$coef, object$se, object$coef/object$se)
    res <- list(coefficients = coef, it = object$it, call = object$call)
    class(res) <- "summary.Rnl"
    res
}
