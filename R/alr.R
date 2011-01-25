alr <-
function(xx, j = NULL, ifclose = FALSE, ifwarn = TRUE)
{
     # Function to compute arithmetic log-ratios of a data matrix in order
     # to 'open' it; the j-th element is used as the divisor and the column
     # for that element is dropped and a matrix returned.
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # variables 'x' must be run through ltdl.fix.df to convert any <dl -ve
     # values to positive half that value, and set zero2na = TRUE if it is
     # required to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     if(!is.matrix(xx)) stop("  ", deparse(substitute(xx)), " is not a Matrix")
     # Remove any rows containing NAs
     temp.x <- remove.na(xx)
     x <- temp.x$x
     p <- temp.x$m
     if(ifwarn) cat("  ** Are the data all in the same measurement units? **\n" )
     if(is.null(j)) stop("  ** The divisor must be specified **")
     if(j > p) stop("j cannot be >", p)
     if(ifclose) x <- 100 * sweep(x, 1, rowSums(x), "/")
     x <- log(x)
     x <- sweep(x, 1, x[, j], "-")
     x <- as.matrix(x[,  - j])
     return(x = x)
}

