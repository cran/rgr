`shape` <-
function(xx, xlab = deparse(substitute(xx)), log = FALSE, xlim = NULL, nclass = "scott", ifbw = FALSE, wend = 
     0.05, colr = 8, ifnright = TRUE, ...)
{
     # Function to plot a graphical EDA summary for a variable, consisting of the 
     # display of a histogram, an empirical cumulative distribution function (ECDF), a
     # a cumulative normal percentage probability (CPP) plot and the corresponding
     # Tukey boxplot or box-and-whisker plot (ifbw = TRUE). Options for histograms are:
     # "sturges", "scott" and "fd" for Friedman-Diaconis.  Setting log = TRUE result
     # in log scaling of the plots and in a log transform prior to calculations for the
     # Tukey boxplot.  The box is infilled with the default grey as used in the
     # histogram (colr = 8).  If no colour is desired set colr = 0.  If a plotting
     # symbol other than a plus (pch=3), set pch = the appropriate integer.  Setting
     # ifnright = FALSE causes the sample size annotation for the histogram to be
     # plotted top left rather than top right.  Note, the boxplot, ECDF and CPP
     # plot x-axes are forced to be the same as for the histogram to aid visual
     # comparison.
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # variable, 'x', data must be run through ltdl.fix.df to convert any <dl
     # -ve values to positive half that value, and set zero2na = TRUE if it is
     # required, to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     frame()
     oldpar <- par()
     on.exit(par(oldpar))
     par(mfrow = c(2, 2), pty = "m", cex.main = 0.9)
     temp.x <- remove.na(xx)
     x <- temp.x$x[1:temp.x$n]
     save <- gx.hist(x, xlab = xlab, ylab = "", log = log, xlim = xlim, main = "Histogram",
         nclass =  nclass, colr = colr, ifnright = ifnright)
     xlim <- save$xlim
     if(ifbw)
         banner <- "Box and Whisker Plot"
     else banner <- "Tukey Boxplot"
     bxplot(x, xlab = xlab, log = log, xlim = xlim, main = banner, ifbw = ifbw, col = colr,
         wend = wend)
     gx.ecdf(x, xlab = xlab, ylab = "", log = log, xlim = xlim, main = 
         "Empirical Cumulative Distribution\nFunction (ECDF)", ...)
     cnpplt(x, xlab = xlab, ylab = "", log = log, xlim = xlim, main = 
         "% Cumulative (Normal) Percentage\nProbability (CPP) Plot", ifshape = TRUE, ...)
     invisible()
}

