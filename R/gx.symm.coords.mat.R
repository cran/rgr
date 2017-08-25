gx.symm.coords.mat <-
function (x, v1, v2) 
{
    # NOTE: Prior to using this function the data frame/matrix containing the
    # parts, x, must be run through ltdl.fix.df to convert any <dl -ve
    # values to positive half that value, and set zero2na = TRUE if it is
    # required to convert any zero values or other numeric codes representing 
    # blanks to NAs.  Parts in the data frame/matrix, x, must be in the 
    # same units.
    #
    if (is.data.frame(x)) x <- as.matrix(x)
    if (any(x < 0)) stop("negative values not allowed\n")
    cat("  ** Are the data/parts all in the same measurement units? **\n")
    vnames <- c(dimnames(x)[[2]][v1], dimnames(x)[[2]][v2])
    #
    x.v1 <- x[, v1]; x.v2 <- x[, v2]
    xx <- x[, -c(v1, v2)]
    xx <- cbind(x.v1, x.v2, xx)
    zz <- gx.symm.coords(xx)
    z <- zz[, c(1:2)]
    dimnames(z)[[2]] <- vnames
    return(z = z)
}
