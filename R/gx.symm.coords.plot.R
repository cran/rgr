gx.symm.coords.plot <-
function (x, v1, v2, log = TRUE, method = "spearman", example = " ", ...) 
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
    xname <- dimnames(x)[[2]][v1]; yname <- dimnames(x)[[2]][v2]
    if (log) iflog <- "xy"
        else iflog <- ""
    frame()
#
    x.v1 <- x[, v1]; x.v2 <- x[, v2]
    xx <- x[, -c(v1, v2)]
    xx <- cbind(x.v1, x.v2, xx)
    z <- gx.symm.coords(xx)
    z.r <- cor(z[, 1], z[, 2], method = method)
    if (log) x.r <- cor(log(x.v1), log(x.v2), method = method)
        else x.r <- cor(x.v1, x.v2, method = method)
#
    if(example == "explore" | example == "Explore") {
        old.par <- par(); on.exit(par(old.par))
        par(mfrow = c(1, 2), cex.main = 0.8, pty = "s")
        plot(x.v1, x.v2, log = iflog, xlab = xname, ylab = yname,
            main = paste("'", method, "' correlation = ", round(x.r, 2), 
            sep = ""))
        plot(z, xlab = paste("Symmetric coordinate for", xname),
            ylab = paste("Symmetric coordinate for", yname),
            main = paste("'", method, "' correlation = ", round(z.r, 2),
            sep = ""))
            invisible()
    }
    else {
        plot(z, xlab = paste("Symmetric coordinate for", xname),
            ylab = paste("Symmetric coordinate for", yname), ...)
        cat(paste("  ", xname, " - ", yname, " '", method, 
            "' correlation for symmetric coordinates = ", 
            round(z.r, 2), "\n", sep = ""))
    }
}
