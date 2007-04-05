`caplot` <-
function(x, y, z, zname = deparse(substitute(z)), caname = deparse(substitute(z)),
     log = FALSE, ifjit = FALSE, ifrev = FALSE, ngrid = 100, colr = topo.colors(16),
     xcoord = "Easting", ycoord = "Northing")
{
     # Original wrapper written by Graeme Bonham-Carter, April 2004, to prepare
     # a concentration-area plot.
     # 
     # Input data consist of x, y & z, where x & y are coordinates on a plane,
     # and z is the measured value at location (x,y).
     #
     # The function uses Akima's (1978) bivariate interpolation procedure from
     # the akima library available in R.  By default a 100 by 100 grid of
     # interpolated points, ngrid = 100, is estimated.  Points outside the
     # convex hull of the observations are set to NA for later removal.  To
     # estimate area the function assumes that the count of grid points is
     # proportional to area.  To result in a reasonable model the observations
     # should be 'evenly' spread over the plane.  If log = TRUE the data are
     # log-transformed prior to interpolation.  If ifjit = TRUE the x and y
     # coordinates are jittered so that no duplicate locations exist, which
     # can cause the interpolation function to fail.  If ifrev= TRUE the
     # empirical concentration-area function is plotted from lowest value to
     # highest.  The direction of accumulation can be key in detecting multi-
     # fractal patterns, it is advantageous to prepare plots with ifrev = both
     # TRUE and FALSE (default).  The plot x-axes are titled with zname, this
     # can be set to "" and no title is displayed.  The interpolated 'map' may
     # be titled by defining caname, often with the text same that would be
     # used for x-axis titling; if no title is required set caname = "".
     #
     # The default colour scheme for the image of interpolated values is set to
     # topo.colours(16).  This results in low values plotting in 'cool' deep
     # purple and the highest values in white 'heat' after progressing through
     # blues, greens and yellows in 16 steps. Other options are: heat.colors,
     # terrain.colors and cm.colors.  To obtain a black and white 8 step grey-
     # scale for publication, set colr = grey(0:8/8).
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # x, y, and z data must be run through ltdl.fix.df to convert any <dl
     # -ve values to positive half that value, and set zero2na = TRUE if it is
     # required, to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     # The akima Library must be attached to access interp, and the V&R MASS
     # Library must be attached to access eqscplot.
     #
     frame()
     oldpar <- par()
     on.exit(par(oldpar))
     par(mfrow = c(2, 2), pty = "m", cex.main = 0.8)
     u <- na.exclude(cbind(x, y, abs(z)))
     xlim <- range(u[, 3])
     cnpplt(u[, 3], xlab = zname, log = TRUE, xlim = xlim, main = 
         "% Cumulative Probability Plot\nOriginal Data", cex.axis = 1, ifshape = TRUE)
     if(ifjit) {
         u[, 1] <- jitter(u[, 1], 0.5)
         u[, 2] <- jitter(u[, 2], 0.5)
     }
     zlgnd <- deparse(substitute(z))
     if(log) {
         u[, 3] <- log10(u[, 3])
         zlgnd <- paste("Log10\n", zlgnd)
     }
     xo <- seq(min(u[, 1]), max(u[, 1]), length.out = ngrid) 
     yo <- seq(min(u[, 2]), max(u[, 2]), length.out = ngrid) 
     new <- interp(u[, 1], u[, 2], u[, 3], xo = xo , yo = yo)
     znew <- na.omit(as.vector(new$z))
     if(log)
         znew <- 10^znew
     cnpplt(as.vector(znew), xlab = zname, log = TRUE, xlim = xlim, main = 
         "% Cumulative Probability Plot\nGridded Data", cex.axis = 0.8, ifshape = TRUE)
     # frame(), so that function will plot correctly under R
     eqscplot(range(new$x), range(new$y), plot = "n", xlab = xcoord, ylab = ycoord,
          main = caname, pch = 32)
     image(new, add = TRUE, col = colr)
     # image.legend(new, horizontal = FALSE, xlab = zlgnd), image.legend call not in R
     conc <- znew[order(znew)]
     cumarea <- seq(1, length(znew))/length(znew) * 100
     if(!ifrev) conc <- rev(conc)
     plot(conc, cumarea, log = "xy", xlab = zname, ylab = "Cumulative area (%)",
          main = "Concentration-Area Plot", xlim = xlim, yaxt = "n", pch = 3)
     axis(2, at = c(0.01, 0.1 ,1 ,10, 100), labels = c("0.01", "0.1", "1", "10", "100"),
          las = 1, cex = 1) 
     limits <- par("usr")
     xpos <- 10^(limits[2] - (limits[2] - limits[1]) * 0.05)
     if(ifrev)
         ypos <- 10^(limits[3] + (limits[4] - limits[3]) * 0.11)
     else ypos <- 10^(limits[4] - (limits[4] - limits[3]) * 0.11)
     text(xpos, ypos, labels = paste("N =", length(conc)), adj = 1)
     invisible()
}

