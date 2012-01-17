xyplot.z <-
function(xx, yy, zz, p = 0.5, sfact = 2.5, zmin = NA, zmax = NA, log = NULL, 
     xlim = NULL, ylim = NULL, xlab = deparse(substitute(xx)), 
     ylab = deparse(substitute(yy)), zlab = deparse(substitute(zz)), main = "",
     symcolr = 1, ifparams = FALSE, iflgnd = FALSE,
     title = deparse(substitute(zz)), ...)
{
     # Function to plot a third variable, zz, as increasing sized circles in
     # the space of a x-y plot.  The rate of increase of the diameter in
     # proportion to the value of zz is controlled by the value of p.  See
     # function syms.pfunc for a plot showing the result of changes in values
     # of p over the normalized 0 to 1 range of the data.  The final size of 
     # the symbol on plotting is controlled by sfact.  Setting zmin and/or zmax
     # results in the symbol sizes being truncated at those levels, forcing all
     # lower or higher values than those specified by the provided values to be
     # plotted as same sized circles.  Use parameter fg to alter symbol colour.
     # Optionally a legend may be added to the plot, and optionally the symbol
     # plotting parameters may also be added.
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # x, y, and zz data must be run through ltdl.fix.df to convert any <dl -ve
     # values to positive half that value, and set zero2na = TRUE if it is
     # required to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     frame()
     temp.z <- remove.na(cbind(xx, yy, zz))
     x <- temp.z$x[1:temp.z$n, 1]
     y <- temp.z$x[1:temp.z$n, 2]
     z <- temp.z$x[1:temp.z$n, 3]
     nz <- temp.z$n
     if(main == "")
         if(zlab == "")
             banner <- ""
         else banner <- paste("Proportional Symbol Plot for", zlab)
     else banner <- main
     z.min <- min(z)
     z.max <- max(z)
     zrange <- c(zmin, zmax)
     rgz <- syms(z, zrange, p = p)
     if(is.null(log)) log <- ""
     plot(x, y, type = "n", xlab = xlab, ylab = ylab, xlim = xlim, 
         ylim = ylim, log = log, main = banner, ...)
     symbols(x, y, circles = rgz, inches = sfact * 0.05, fg = symcolr, add = TRUE, ...)
     if(iflgnd) {
         if(!is.na(zmax)) z[z > zmax] <- zmax
         if(!is.na(zmin)) z[z < zmin] <- zmin
         zval <- quantile(z, prob = c(1.0, 0.75, 0.5, 0.25, 0.0))
         rgz <- syms(zval, zrange, p = p)
         zval <- signif(zval, 3)
         legend(locator(1), pch = rep.int(1, 5), pt.cex = rgz[1:5] * sfact / 0.8,
             col = rep.int(symcolr, 5), paste(" ", zval[1:5]), cex = 0.8,
             title = title, ...)
     }
     if(ifparams) text(locator(1), paste("p =", signif(p, 3), "& sfact =", sfact,
         "\nz.max =", signif(z.max, 3), "; zmax =", zmax,
         "\nz.min =", signif(z.min, 3), "; zmin =", zmin),
         cex = 0.8, adj =c(0, 1))
     invisible()
}

