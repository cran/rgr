"edamap8" <-
function(x, y, zz, sfact = 1, xlab = "Easting", ylab = "Northing", zlab = deparse(
     substitute(zz)), main = "", ifgrey = FALSE, symcolr = NULL, tol = 0.04)
{
     # Function to plot an EDA map where the data are divided into 8 groups based
     # on divisions at the 2nd, 5, 25, 50, 75, 95 and 98th percentiles; data below
     # the median are plotted as increasing sized circles, and data above the median
     # as increasing sized squares.  The variable sfact is used to scale the symbols
     # as appropriate for the display device.  The default set of colours are 
     # inceasingly deeper blues for <Q2, green for between Q2 and Q3, and oranges
     # and reds for >Q3.  Alternately a grey-scale map may be generated.
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # x, y, and z data must be run through ltdl.fix.df to convert any <dl
     # -ve values to positive half that value, and set zero2na = TRUE if it is
     # required, to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     # The V&R MASS Library must be attached to access eqscplot.
     #
     frame()
     oldpar <- par()
     on.exit(par(oldpar))
     par(pty = "m")
     temp.z <- remove.na(zz)
     z <- temp.z$x[1:temp.z$n]
     nz <- temp.z$n
     if(main == "")
         if(zlab == "")
          banner <- ""
         else banner <- paste("EDA Percentile Based Map for", zlab)
     else banner <- main
     eqscplot(x, y, type = "n", xlab = xlab, ylab = ylab, main = banner, tol = tol)
     zcut <- quantile(z, probs = c(0.02, 0.05, 0.25, 0.5, 0.75, 
         0.95, 0.98))
     zzz <- cutter(z, zcut)
     npch <- c(1, 1, 1, 1, 0, 0, 0, 0)
     size <- c(2, 1.5, 1, 0.5, 0.5, 1, 1.5, 2) * sfact
     if(ifgrey) {
         symcolr <- grey(c(0, 0.15, 0.3, 0.4, 0.4, 0.3, 0.15, 0))
     }
     else {
         palette(rainbow(36))
         if(length(symcolr) != 8) symcolr <- c(25, 22, 20, 13, 13, 6, 4, 1)
     }
     for(i in 1:nz) {
         points(x[i], y[i], pch = npch[zzz[i]], cex = size[zzz[i]], col = symcolr[zzz[i]])
     }
     cat("\tCut Levels\t No. of Symbols    Symbol & cex & Colour\n\t\t\t\t\t\tsfact =",
         format(sfact, nsmall=2), "\n")
     stype <- character(8)
     stype[1:4] <- "Circle"
     stype[5:8] <- "Square"
     pct <- 0
     for(i in 1:7) {
         ni <- length(zzz[zzz == i])
         pct <- pct + 100 * ni/nz
         cat("\t\t\t      ", ni, "\t    ", stype[i], format(size[i], nsmall = 2),
             "  ", symcolr[i], "\n\t", signif(zcut[i], 4), "\t", round(pct, 1), "%\n")
     }
     ni <- length(zzz[zzz == 8])
     cat("\t\t\t      ", ni, "\t    ", stype[8], format(size[8], nsmall = 2),
         "  ", symcolr[8], "\n")
     palette("default")
     invisible()
}

