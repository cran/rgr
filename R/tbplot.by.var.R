tbplot.by.var <-
function(xmat, log = FALSE, logx = FALSE, notch = FALSE, xlab = "Measured Variables",
	ylab = "Reported Values", main = "", label = NULL, plot.order = NULL, 
	xpos = NA, las = 1, cex = 1, adj = 0.5, colr = 8, ...)
{
     # Function to plot multiple variables from a dataframe or matrix, as distinct
     # from subsets for a single variable, as Tukey boxplots; the variables are
     # displayed in alpabetical order, use plot.order to re-arrange as wished.  
     # See function tbplot for other "cosmetic" adjustments.
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # variable, 'x', data must be run through ltdl.fix.df to convert any <dl
     # -ve values to positive half that value, and set zero2na = TRUE if it is
     # required, to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     zz <-var2fact(xmat)
     x <- zz[, 1]
     y <- as.numeric(zz[, 2])
     if(is.null(label))
         label <- sort(unique(x))
     tbplot(split(y, x), log = log, logx = logx, notch = notch, xlab= xlab,
         ylab = ylab, main = main, label = label, plot.order = plot.order,
         xpos = xpos, las =las, cex = cex, adj = adj, colr = colr, ...)
     invisible()
}

