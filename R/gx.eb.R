gx.eb <-
function(r, s, xx, ...)
{
    # NOTE: Prior to using this function the data frame/matrix containing the
    # parts, xx, must be run through ltdl.fix.df to convert any <dl -ve
    # values to positive half that value, and set zero2na = TRUE if it is
    # required to convert any zero values or other numeric codes representing 
    # blanks to NAs.  Parts in the data frame/matrix, xx, must be in the 
    # same units, any compositions including NAs will be removed.
    #
    if (is.data.frame(xx)) xx <- as.matrix(xx)
    if (any(xx < 0, na.rm = TRUE)) stop("Negative values not allowed\n")
    cat("  ** Are the data/parts all in the same measurement units? **\n")
    temp.x <- remove.na(xx, iftell = FALSE)
    x <- temp.x$x; nna <- temp.x$nna
    if (nna >= 1) cat(" ", nna, "composition(s) with NA(s) removed\n")
    #
    ListOfParts <- list(...)
    kk <- length(ListOfParts)
    if(kk != (r + s)) stop("Sum of parts in numerator and denominator must ",
        "equal length of List of Parts\n")
    # 
    num.parts <- unlist(ListOfParts[1 : r])
    num.names <- colnames(x)[num.parts]
    cat("  Parts in numerator:", num.names, "\t[", num.parts, "]\n")
    num <- x[, num.parts]
    num <- log(num)
    num.mean <- rowMeans(num)
    #
    den.parts <- unlist(ListOfParts[(r+1) : (r+s)])
    den.names <- colnames(x)[den.parts]
    cat("  Parts in denomintor:", den.names, "\t[", den.parts, "]\n")
    den <- x[, den.parts]
    den <- log(den)
    den.mean <- rowMeans(den)
    #
    temp <- sqrt(r * s / (r + s))
    z <- temp * exp(num.mean - den.mean)
    #
    return(z = z)
}
