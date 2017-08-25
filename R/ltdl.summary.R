ltdl.summary <-
function (xmat, vars = NULL, coded = -9999) 
{
    xmat.name <- deparse(substitute(xmat))
    if (!is.data.frame(xmat)) 
        stop(paste("  ", xmat.name, "is not a data frame"))
    xsav <- xmat
    ind.num <- sapply(xmat, is.numeric)
    xmat <- as.matrix(xmat[, ind.num])
#
    if(is.null(vars)) {
        xname <- dimnames(xmat)[[2]]
        nvars <- dim(xmat)[2]
        vars <- seq(1:nvars)
    }
    else {
        nvars <- length(vars)
        xname <- character(nvars)
        varnums <- integer(nvars)
        for (i in 1:nvars) {
            ii <- vars[i]
            if (is.numeric(vars[i])) xname[i] <- dimnames(xmat)[[2]][ii]
            else xname[i] <- vars[i]
        }
    }
    cat("  Data frame:", xmat.name, "\n")    
    cat("  Variable        N     NA     <DL (-ve)", coded, "\n\n")
#
    for (i in 1:nvars) {
        ii <- vars[i]
        x <- xmat[, ii]
        n <- length(x)
        ncoded <- length(x[!is.na(x) & x == coded])
        nna <- length(x[is.na(x)])
        nneg <- length(x[x < 0]) - nna - ncoded
        cat(" ", xname[i], "    \t", n, "\t", nna, "\t", nneg, "\t   ",
            ncoded, "\n")    }
    invisible()
}
