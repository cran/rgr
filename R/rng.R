rng <-
function(xx) 
{
     # Function to undertake a range transformation on a data matrix in
     # order that each column is scaled zero-one.
     #
     if(!is.matrix(xx)) stop(deparse(substitute(xx)), " is not a Matix")
     # Remove any rows containing NAs
     temp.x <- remove.na(xx)
     x <- temp.x$x
     p <- temp.x$m
     xmin <- numeric(p); xdiff <- numeric(p)
     for (j in 1:p) {
         xr <- range(x[, j])
         xmin[j] <- xr[1]
         xdiff[j] <- diff(xr)
     }
     x <- sweep(x, 2, xmin, "-")
     x <- sweep(x, 2, xdiff, "/")
     return(x = x)
}

