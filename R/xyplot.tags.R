xyplot.tags <-
function(xx, yy, tag, log = NULL, xlim = NULL, ylim = NULL, 
         xlab = deparse(substitute(xx)), 
         ylab = deparse(substitute(yy)),
         taglab = deparse(substitute(tag)), main = "", ...)
{
     # Function to display tags or sample IDs in the space of an x-y plot; for
     # sample IDs use: plot.tags(x,y,dimnames(df)[[1]],cex=0.8).
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # x, y data must be run through ltdl.fix.df to convert any <dl -ve values
     # to positive half that value, setting zero2na = TRUE if it is required
     # to convert any zero values or other numeric codes representing blanks
     # to NAs.
     # 
     frame()
     temp.x <- remove.na(cbind(xx, yy))
     x <- temp.x$x[1:temp.x$n, 1]
     y <- temp.x$x[1:temp.x$n, 2]
     if(main == "")
         if(taglab == "")
             banner <- ""
         else banner <- paste("Plot of 'values' for", taglab)
     else banner <- main
     tag[is.na(tag)] <- "+"
     if(is.null(log)) log <- ""
     plot(x, y, log = log, xlim = xlim, ylim = ylim, type = "n", 
         xlab = xlab, ylab = ylab, main = banner, ...)
     text(x, y, tag, ...)
     invisible()
}

