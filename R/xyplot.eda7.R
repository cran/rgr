xyplot.eda7 <-
function(xx, yy, zz, sfact = 1, xlim = NULL, ylim = NULL, log = NULL,
         logz = FALSE, xlab = deparse(substitute(xx)),
         ylab = deparse(substitute(yy)),
         zlab = deparse(substitute(zz)), main = "",
         ifgrey = FALSE, symcolr = NULL, ...)
{
     # Function to plot a third variable, zz, as an EDA symbol where the data are
     # divided into 7 groups based on a Tukey boxplot.  Data between Q1 and Q3 are
     # plotted as crosses while data in the whiskers, near and far outliers below
     # Q1 are plotted as increasing sized circles, and as increasing sized squares
     # above Q3.  The variable sfact is used to scale the symbols as appropriate 
     # for the display device.  The default set of colours from the rainbow(36)
     # pallette are inceasingly deeper blues for<Q2, green for between Q2 and Q3,
     # and oranges and reds for >Q3 (see display.rainbow()).  Alternately a
     # grey-scale map may be generated.
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # x, y, and zz data must be run through ltdl.fix.df to convert any <dl
     # -ve values to positive half that value, and set zero2na = TRUE if it is
     # required to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     frame()
     oldpar <- par()
     on.exit(par(oldpar))
     temp.z <- remove.na(cbind(xx, yy, zz))
     x <- temp.z$x[1:temp.z$n, 1]
     y <- temp.z$x[1:temp.z$n, 2]
     z <- temp.z$x[1:temp.z$n, 3]
     nz <- temp.z$n
     if(main == "")
         if(zlab == "")
             banner <- ""
         else {
             if(logz) banner <-
                        paste("EDA Tukey Boxplot Based Plot for Log10", zlab)
         else banner <- paste("EDA Tukey Boxplot Based Plot for", zlab)
         }
     else banner <- main
     if(is.null(log)) log = ""
     plot(x, y, type = "n", log = log, xlab = xlab, ylab = ylab,
         xlim = xlim, ylim = ylim, main = banner, ...)
     if(logz) z <- log10(z)
     q <- quantile(z, probs = c(0.25, 0.75))
     zcut <- numeric(6)
     hw <- q[2] - q[1]
     zcut[1] <- q[1] - 3 * hw
     zcut[2] <- q[1] - 1.5 * hw
     zcut[3] <- q[1]
     zcut[4] <- q[2]
     zcut[5] <- q[2] + 1.5 * hw
     zcut[6] <- q[2] + 3 * hw
     zzz <- cutter(z, zcut)
     npch <- c(1, 1, 1, 3, 0, 0, 0)
     size <- c(2, 1.5, 1, 0.5, 1, 1.5, 2) * sfact
     if(ifgrey) {
         symcolr <- grey(c(0, 0.15, 0.3, 0.4, 0.3, 0.15, 0))
     }
     else {
         palette(rainbow(36))
         if(length(symcolr) != 7) symcolr <- c(25, 22, 20, 13, 6, 4, 1)
     }
     for(i in 1:nz) {
         points(x[i], y[i], pch = npch[zzz[i]], cex = size[zzz[i]], col = symcolr[zzz[i]])
     }
     cat("\tCut Levels\t  No. of Symbols   Symbol - size - Colour\n\tLog =", logz, 
         "\t\t\t\tsfact =", format(sfact, nsmall = 2), "\n\n")
     stype <- character(7)
     stype[1:3] <- "Circle "
     stype[4] <- "Cross  "
     stype[5:7] <- "Square "
     for(i in 1:6) {
         if(logz) zcut[i] <- 10^zcut[i]
         ni <- length(zzz[zzz == i])
         cat("\t\t\t      ", ni, "\t    ", stype[i], format(size[i], nsmall = 2),
             "  ", symcolr[i], "\n\t", round(zcut[i], 2), "\n")
     }
     cat("\t\t\t      ", length(zzz[zzz == 7]), "\t    ", stype[7], format(size[7],
         nsmall = 2), "  ", symcolr[7], "\n")
     palette("default")
     invisible()
}

