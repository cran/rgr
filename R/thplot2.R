thplot2 <-
function(x, xname = deparse(substitute(x)), ifzero = 0.01, xlow = NA, xhih = NA,
     yhih = NA, rsd = 5, ptile = 95, main = "", ifalt = FALSE, ...)
{
     # Function to display a Thompson-Howarth plot for duplicate measurements.
     # Duplicate measurements are input as a single vector: X[1], ... X[n]
     # followed by X[n+2], ... X[2n],or alternated as x[1] and x[2] being a
     # pair through to x[2*i+1] and x[2*i+2], for the i in 1:n duplicate pairs,
     # from a matrix or dataframe.  The function creates two seperate vectors:
     # X1[1], ... X1[n] and X2[1], ... X2[n] and passes them to function
     # thplot1.
     #
     # If the data are as two variables from a matrix or data frame use thplot1.
     #
     # Note: This function expects the RSD%, which is more familiar to the
     # current generation of applied geochemists, rather than the precision
     # at the 2 Std. Deviation level.  The necessary calculations to conform
     # with the Thompson and Howarth procedure are made internally.
     #
     n <- length(x)
     ndup <- n/2
     x1 <- numeric(ndup)
     x2 <- numeric(ndup)
     #
     if(ifalt) {
         for(i in 1:ndup) {
             j <- 2 * (i - 1) + 1
             x1[i] <- x[j]
             x2[i] <- x[j + 1]
         }
     }
     else {
         for(i in 1:ndup) {
             x1[i] <- x[i]
             x2[i] <- x[ndup + i]
         }
     }
     #
     thplot1(x1, x2, xname = xname, ifzero = ifzero, xlow = xlow, xhih = xhih,
         yhih = yhih, rsd = rsd, ptile = ptile, main = main, ...)
     invisible()
}

