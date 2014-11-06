hlest <-
function (x) 
{
    xpairs = pairup(x, type = "leq")
    was = (xpairs[, 1] + xpairs[, 2])/2
    hlest = median(was)
    hlest
}
