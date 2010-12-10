inset <-
function(xx, xlab = deparse(substitute(xx)), log = FALSE, xlim = NULL, nclass = NULL,
         ifnright = TRUE, table.cex = 0.7, ...)
{
     # Function to plot a graphical EDA summary to be used as an inset
     # on a geochemical map or in a report; a histogram and a cumulative
     # normal percentage probability plot (CPP) are displayed, together
     # with selected summary statistics.  In addition, the summary
     # statustics are displayed on the on the current device.
     #
     # Setting log = TRUE generates log-scaled scaled plots.  Options for
     # histograms, nclass, are: "scott", "fd" (Friedman-Diaconis), and
     # "sturges".  If nclass is omitted, "scott" is used for N < 500, 
     # "fd" for N >= 500.
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # variable, 'xx', data must be run through ltdl.fix.df to convert any <dl
     # -ve values to positive half that value, and set zero2na = TRUE if it is
     # required, to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     oldpar <- par()
     on.exit(par(oldpar))
     par(mfrow = c(1, 3), pty = "s")
     temp.x <- remove.na(xx)
     x <- temp.x$x[1:temp.x$n]
     stats <- gx.stats(x, display = FALSE)
     nobs <- stats$stats[20]
     if((is.null(nclass)) && (nobs < 500))
         nclass <- "scott"
     else nclass <- "fd"
     save <- gx.hist(x, xlab = xlab, ylab = "", log = log, xlim = xlim, main = "Histogram",
         nclass = nclass, colr = 8, ifnright = ifnright, cex = 0.8, ...)
     xlim <- save$xlim 
     frame()
     fms <- par("mai")
     par(mai = c(0.1, 0.1, 0.4, 0.1))
     title(" Summary Statistics")
     x1 <- -0.57
     x2 <- 1.25
     yy <- 1.1
     ydec <- 0.11
     text(x1, yy, paste("Maximum"), cex = table.cex, adj = 0)
     text(x2, yy, stats$stats[19], cex = table.cex, adj = 1)
     yy <- yy - ydec
     text(x1, yy, paste("98th Percentile"), cex = table.cex, adj = 0)
     text(x2, yy, stats$stats[17], cex = table.cex, adj = 1)
     yy <- yy - ydec
     text(x1, yy, paste("95th Percentile"), cex = table.cex, adj = 0)
     text(x2, yy, stats$stats[16], cex = table.cex, adj = 1)
     yy <- yy - ydec
     text(x1, yy, paste("90th Percentile"), cex = table.cex, adj = 0)
     text(x2, yy, stats$stats[15], cex = table.cex, adj = 1)
     yy <- yy - ydec
     text(x1, yy, paste("3rd Quartile"), cex = table.cex, adj = 0)
     text(x2, yy, stats$stats[13], cex = table.cex, adj = 1)
     yy <- yy - ydec
     text(x1, yy, paste("Median"), cex = table.cex, adj = 0)
     text(x2, yy, stats$stats[10], cex = table.cex, adj = 1)
     yy <- yy - ydec
     text(x1, yy, paste("1st Quartile"), cex = table.cex, adj = 0)
     text(x2, yy, stats$stats[7], cex = table.cex, adj = 1)
     yy <- yy - ydec
     text(x1, yy, paste("10th Percentile"), cex = table.cex, adj = 0)
     text(x2, yy, stats$stats[5], cex = table.cex, adj = 1)
     yy <- yy - ydec
     text(x1, yy, paste("5th Percentile"), cex = table.cex, adj = 0)
     text(x2, yy, stats$stats[4], cex = table.cex, adj = 1)
     yy <- yy - ydec
     text(x1, yy, paste("2nd Percentile"), cex = table.cex, adj = 0)
     text(x2, yy, stats$stats[2], cex = table.cex, adj = 1)
     yy <- yy - ydec
     text(x1, yy, paste("Minimum"), cex = table.cex, adj = 0)
     text(x2, yy, stats$stats[1], cex = table.cex, adj = 1)
     yy <- yy - ydec - 0.045
     text(x1, yy, paste("Median Abs. Deviation"), cex = table.cex, adj = 0)
     text(x2, yy, signif(stats$stats[21], 3), cex = table.cex, adj = 1)
     yy <- yy - ydec
     text(x1, yy, paste("IQR Est. of Std. Dev."), cex = table.cex, adj = 0)
     text(x2, yy, signif(stats$stats[22], 3), cex = table.cex, adj = 1)
     yy <- yy - ydec - 0.045
     text(x1, yy, paste("Mean"), cex = table.cex, adj = 0)
     text(x2, yy, signif(stats$stats[23], 3), cex = table.cex, adj = 1)
     yy <- yy - ydec
     text(x1, yy, paste("Standard Deviation"), cex = table.cex, adj = 0)
     text(x2, yy, signif(stats$stats[25], 3), cex = table.cex, adj = 1)
     yy <- yy - ydec
     text(x1, yy, paste("Coeff. of Variation, %"), cex = table.cex, adj = 0)
     text(x2, yy, signif(stats$stats[26], 3), cex = table.cex, adj = 1)
     par(mai = fms)
     cnpplt(x, xlab = xlab, ylab = "", log = log, xlim = xlim,
         main = "% Probability Plot", ifshape = TRUE, ...)
     invisible()
}

