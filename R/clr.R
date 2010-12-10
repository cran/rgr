clr <-
function(xx, ifclose = FALSE)
{
     # Function to centre log-ratio transform a data matrix in order to 'open'
     # it.
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # variables 'x' must be run through ltdl.fix.df to convert any <dl -ve
     # values to positive half that value, and set zero2na = TRUE if it is
     # required to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     if(!is.matrix(xx)) stop(deparse(substitute(xx)), " is not a Matix\n")
     temp.x <- remove.na(xx)
     x <- temp.x$x
     if(ifclose) x <- 100 * sweep(x, 1, rowSums(x), "/")
     x <- log(x)
     x <- sweep(x, 1, rowMeans(x), "-")
     x <- as.matrix(x)
     return(x = x)
}

