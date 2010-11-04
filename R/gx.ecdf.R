gx.ecdf <-
function(xx, xlab = deparse(substitute(xx)), ylab = "Empirical Cumulative Distribution Function", 
     log = FALSE, xlim = NULL, main = " ", pch = 3, ifqs = FALSE)
{
     # Function to plot the plain vanilla empirical cumulative distribution function
     # (ECDF) for a variable.  Optionally the data may be presented with log scaling
     # and the quartiles plotted.
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # variable, 'xx', data must be run through ltdl.fix.df to convert any <dl
     # -ve values to positive half that value, and set zero2na = TRUE if it is
     # required, to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     temp.x <- remove.na(xx)
     x <- sort(temp.x$x[1:temp.x$n])
     nx <- temp.x$n
     y <- ((1:nx) - 0.5)/nx
     if(log) {
         logx <- "x"
         if((!is.null(xlim)) && (xlim[1] <= 0))
          xlim[1] <- min(x)
     }
     else logx <- ""
     if(is.null(xlim)) {
         plot(x, y, log = logx, xlab = xlab, ylab = ylab, main = main, pch = pch, las = 1)
         limits <- par("usr")
         nxx <- nx
     }
     else {
         xt <- x[(x >= xlim[1]) & (x <= xlim[2])]
         yt <- y[(x >= xlim[1]) & (x <= xlim[2])]
         plot(xt, yt, log = logx, xlim = xlim, xlab = xlab, ylab = ylab, main = main,
             pch = pch, las = 1)
         limits <- par("usr")
         nxx <- length(xt)
     }
     if(ifqs) {
         abline(h = 1:3/4, lty = 3)
         abline(v = quantile(x, probs = c(0.25, 0.5, 0.75)), lty = 3)
     }
     xpos <- limits[2] - (limits[2] - limits[1]) * 0.05
     ypos <- limits[3] + (limits[4] - limits[3]) * 0.15
     if(log)
         xpos <- 10^xpos
     text(xpos, ypos, labels = paste("N =", nx), adj = 1)
     if(nxx != nx) {
         ypos <- limits[3] + (limits[4] - limits[3]) * 0.06
         text(xpos, ypos, labels = paste(nx - nxx, "points omitted"), adj = 1)
     }
     invisible()
}

