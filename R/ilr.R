ilr <-
function(xx, ifclose = FALSE, ifwarn = TRUE)
{
    # Function to compute an isometric log-ratio transformation to 'open'
    # a n x p 'closed' matrix, the result is a n x (p-1) matrix.  Based
    # on a script by P. Filzmoser & K. Hron, Math. Geosci. 40(3):234-248.
    #
    if (is.data.frame(xx)) xx <- as.matrix(xx)
    if (any(xx < 0, na.rm = TRUE)) stop("Negative values not allowed\n")
    if(ifwarn) cat("  ** Are the data/parts all in the same measurement units? **\n")
    temp.x <- remove.na(xx, iftell = FALSE)
    x <- temp.x$x; n <- temp.x$n; p <- temp.x$m; nna <- temp.x$nna
    if (nna >= 1) cat(" ", nna, "composition(s) with NA(s) removed\n")
    #   
    if(ifclose) x <- 100 * sweep(x, 1, rowSums(x), "/")
    #
    x.iso <- matrix(NA, n, p-1)
    for (i in 1:n) {
        for (k in 1:(p-1)) {
            x.iso[i, k] <- sqrt(k/(k+1))*log(((prod(x[i, 1:k]))^(1/k))/x[i, k+1])
        }
    }
    dimnames(x.iso)[[1]] <- dimnames(x)[[1]]
    isovars <- character(p-1)
    for (k in 1:(p-1)) isovars[k] <- paste("Iso", k, sep = '')
    dimnames(x.iso)[[2]] <- isovars
    #
    return(x.iso = x.iso)
}
