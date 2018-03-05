alr <-
function (xx, j = NULL, ifclose = FALSE, ifwarn = TRUE) 
{
    if (is.data.frame(xx)) xx <- as.matrix(xx)
    if (any(xx < 0, na.rm = TRUE)) stop("Negative values not allowed\n")
    if(ifwarn) cat("  ** Are the data/parts all in the same measurement units? **\n")
    temp.x <- remove.na(xx, iftell = FALSE)
    x <- temp.x$x; p <- temp.x$m; nna <- temp.x$nna
    if (nna >= 1) cat(" ", nna, "composition(s) with NA(s) removed\n")
    #
    if (is.null(j)) stop("  ** The divisor must be specified **")
    if (j > p) stop("j cannot be >", p)
    cat ("  The divisor is", colnames(xx)[j], "\n")
    if (ifclose) x <- 100 * sweep(x, 1, rowSums(x), "/")
    #
    x <- log(x)
    x <- sweep(x, 1, x[, j], "-")
    x <- as.matrix(x[, -j])
    #
    return(x = x)
}
