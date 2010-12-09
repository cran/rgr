wtd.sums <-
function(xx, ri, xloc = NULL, xspread = NULL)
{
     # Function to compute weighted sums for a matrix using medians and MADS,
     # and user-defined relative importances, see: Garrett, RG & Grunsky, EC,
     # 2001. Weighted Sums - knowledge based empirical indices for use in
     # exploration geochemistry, Geochem: Explor. Env. Anal., 1(4):135-141.
     # Alternate estimates of locations/centres and spreads/dispersions may
     # be provided, e.g., with non-robust or other robust estimates.
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # variables 'xx' must be run through ltdl.fix.df to convert any <dl -ve
     # values to positive half that value, and set zero2na = TRUE if it is
     # required to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     if(!is.matrix(xx)) stop(deparse(substitute(xx)), " is not a Matrix")
     temp.x <- remove.na(xx)
     x <- temp.x$x
     ncolx <- temp.x$m
     nlnri <- length(ri)
     if(ncolx != nlnri)
         stop("\n  Number of variables and importances do not match")
     w <- ri/sum(abs(ri))
     a <- w/sqrt(sum(w * w))
     if(is.null(xloc)) xloc <- apply(x, 2, median)
         else {if(length(xloc) != nlnri)
             stop("\n  Numbers of variables and locations do not match")
         }
     if(is.null(xspread)) xspread <- apply(x, 2, mad)
         else {if(length(xspread) != nlnri)
             stop("\n  Numbers of variables and spreads do not match")
         }
     xcentred <- sweep(x, 2, xloc, "-")
     z <- sweep(xcentred, 2, xspread, "/")
     ws <- z %*% a
     invisible(list(input = deparse(substitute(xx)), xloc = xloc, 
         xspread = xspread,  ri = ri, w = w, a = a, ws = ws))
}

