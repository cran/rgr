gx.ilr <-
function (xx, v1, ifwarn = FALSE) 
{
    # NOTE: Prior to using this function the data frame/matrix containing the
    # parts, x, must be run through ltdl.fix.df to convert any <dl -ve
    # values to positive half that value, and set zero2na = TRUE if it is
    # required to convert any zero values or other numeric codes representing 
    # blanks to NAs.  Parts in the data frame/matrix, x, must be in the same
    # units, any NAs will be removed.
    #
    if (is.data.frame(xx)) xx <- as.matrix(xx)
    if (any(xx < 0, na.rm = TRUE)) stop("Negative values not allowed\n")
    if (ifwarn) cat("  ** Are the data/parts all in the same measurement units? **\n")
    temp.x <- remove.na(xx, iftell = FALSE)
    x <- temp.x$x; p <- temp.x$m; nna <- temp.x$nna
    if (nna >= 1) cat(" ", nna, "composition(s) with NA(s) removed\n")
    #
    p1 <- sqrt(p / (p - 1))
    x <- log(x)
    x.v1 <- x[, v1]
    z <- p1 * (x.v1 - rowMeans(x[, -v1]))
    #
    return(z = z)
}
