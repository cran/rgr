anova2 <-
function (x, xname = deparse(substitute(x)), log = FALSE, ifalt = FALSE) 
{
    n <- length(x)
    ndup <- n/2
    x1 <- numeric(ndup)
    x2 <- numeric(ndup)
    if (ifalt) {
        for (i in 1:ndup) {
            j <- 2 * (i - 1) + 1
            x1[i] <- x[j]
            x2[i] <- x[j + 1]
        }
    }
    else {
        for (i in 1:ndup) {
            x1[i] <- x[i]
            x2[i] <- x[ndup + i]
        }
    }
    anova1(x1, x2, xname = xname, log = log)
    invisible()
}
