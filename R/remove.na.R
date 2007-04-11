`remove.na` <-
function(xx)
{
     # Function to remove 'rows' containing NAs from a vector or a matrix
     # and inform the user of how many rows were removed.
     #
     if(is.vector(xx)) {
         nx <- length(xx)
         x <- na.omit(xx)
         n <- length(x)
         m <- 1     
         intype <- "vector"
     }
     else {
         nx <- length(xx[ ,1]) 
         x <- na.omit(xx)
         n <- length(x[ ,1])
         m <- length(x[1, ])
         intype <- "matrix"
     }
     nna <- nx - n
     if(nna > 0) cat(paste(" ", nna, "row(s) with missing value(s), NA(s), removed from", intype, "\n"))
     invisible(list(x = x, n = n, m = m, nna = nna))
}

