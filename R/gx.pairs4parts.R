gx.pairs4parts <-
function (xx, cex = 2, ifwarn = TRUE, ...) 
{
    # NOTE: Prior to using this function the data frame/matrix containing the
    # parts, xx, must be run through ltdl.fix.df to convert any <dl -ve
    # values to positive half that value, and set zero2na = TRUE if it is
    # required to convert any zero values or other numeric codes representing 
    # blanks to NAs.  Parts in the data frame/matrix, xx, should be in the 
    # same units.
    #
    if (!is.matrix(xx)) 
        stop(deparse(substitute(xx)), " is not a Matrix")
    if (ifwarn) 
        cat("  ** Are the data all in the same measurement units? **\n")
    temp.x <- remove.na(xx)
    x <- temp.x$x
    nx <- temp.x$m
    frame()
    old.par <- par(); on.exit(par(old.par))
    par(mfrow = c(nx, nx), pty = "s", mar = c(2, 2, 2, 2), oma = c(0, 
        0, 0, 0))
    for (i in 1:nx) {
        for (j in 1:nx) {
            if (j < i) {
                xy.ilr <- log(x[, i]/x[, j])/1.4142
                ilr.MAD <- mad(xy.ilr)
                ilr.stab <- exp(-ilr.MAD * ilr.MAD)
                bxplot(xy.ilr, main = paste(round(ilr.stab, 2)), 
                  ifn = FALSE, ...)
            }
            if (j == i) {
                xname <- dimnames(xx)[[2]][i]
                plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, 
                  frame.plot = FALSE, ...)
                text(0.5, 0.5, paste(xname), adj = c(0.5, 0.5), 
                  cex = cex, ...)
            }
            if (j > i) 
                plot(x[, j], x[, i], log = "xy", xlab = " ", 
                  ylab = " ", ...)
        }
    }
    invisible()
}
