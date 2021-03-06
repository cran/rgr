shape.alt <-
function (xx, xlab = deparse(substitute(xx)), log = FALSE, xlim = NULL, 
    nclass = NULL, ifnright = TRUE, ifrev = FALSE, colr = 8, ...) 
{
    frame()
    old.par <- par(); on.exit(par(old.par))
    par(mfrow = c(2, 2), pty = "m", cex.main = 0.9)
    temp.x <- remove.na(xx)
    x <- temp.x$x[1:temp.x$n]
    nobs <- temp.x$n
    if ((is.null(nclass)) && (nobs <= 500)) nclass <- "scott"
    if ((is.null(nclass)) && (nobs > 500)) nclass <- "fd"
    save <- gx.hist(x, xlab = xlab, ylab = " ", log = log, xlim = xlim, 
        main = "Histogram", nclass = nclass, ifnright = ifnright, 
        colr = colr, ...)
    xlim <- save$xlim
    cnpplt(x, xlab = xlab, ylab = " ", log = log, xlim = xlim, 
        main = "% Cumulative Percentage\n(Normal) Probability (CPP) Plot", 
        ifshape = TRUE, ...)
    gx.ecdf(x, xlab = xlab, ylab = " ", log = log, xlim = xlim, 
        main = "Empirical Cumulative Distribution\nFunction (ECDF)", 
        ...)
    gx.mf(x, ifrev = ifrev, xlab = xlab, ylab = "Cumulative Percentage of Data",
        main = "Concentration-Number (C-N) Plot", xlim, ...)
    invisible()
}
