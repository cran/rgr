map.tags <-
function(xx, yy, tag, xlab = "Easting", ylab = "Northing", 
         taglab = deparse(substitute(tag)), main = "", tol = 0.04, ...)
{
     # Function to display tags or sample IDs in the geographic space of a map;
     # for sample IDs use: map.tags(x,y,dimnames(df)[[1]],cex=0.8).
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # x, y data must be run through ltdl.fix.df to convert any <dl -ve values
     # to positive half that value, and set zero2na = TRUE if it is required
     # to convert any zero values or other numeric codes representing blanks
     # to NAs.
     #
     # The V&R MASS Library must be attached to access eqscplot.
     #
     frame()
     oldpar <- par()
     on.exit(par(oldpar))
     par(pty = "m")
     temp.x <- remove.na(cbind(xx, yy))
     x <- temp.x$x[1:temp.x$n, 1]
     y <- temp.x$x[1:temp.x$n, 2]
     if(main == "")
         if(taglab == "")
             banner <- ""
         else banner <- paste("Map of 'values' for", taglab)
     else banner <- main
     tag[is.na(tag)] <- "+"
     eqscplot(x, y, type = "n", xlab = xlab, ylab = ylab, main = banner,
         tol = tol, ...)
     text(x, y, tag, ...)
     invisible()
}

