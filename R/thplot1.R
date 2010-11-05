thplot1 <-
function(x1, x2, name = "", ifzero = 0.01, xlow = NA, xhih = NA, yhih = NA,
     rsd = 5, ptile = 95, main = "")
{
     # Function to display Thompson-Howarth plots of duplicate measurements, 
     # see Thompson, M & Howarth, RJ, 1978: A new approach to the estimation
     # of analytical precision, Journal Geochemical Exploration, 9(1):23-30.
     #
     # This script has been published, see Garrett, RG & Grunsky, EC, 2003,
     # S and R functions to display Thompson-Howarth plots.  Computers &
     # Geosciences 29(2):239-242.  Some modifications have been made to plot
     # annotation.
     #
     # Duplicate measurements are input as two vectors: X1[1], ... X1[n] and
     # X2[1], ... X2[n] from a matrix or data frame.
     #
     # Note: This function expects the RSD%, which is more familiar to the
     # current generation of applied geochemists, rather than the precision
     # at the 2 Std. Deviation level.  The necessary calculations to conform
     # with the Thompson and Howarth procedure are made internally.
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # variable, 'x', data must be run through ltdl.fix.df to convert any <dl
     # -ve values to positive half that value, and set zero2na = TRUE if it is
     # required, to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     # If the duplicate measurements are input as a single vector: X[1], ...
     # X[n] followed by X[n+2], ... X[2n], or alternated as X[1] and X[2] being
     # a pair through to x[2*i+1] and x[2*i+2], for the i in 1:n duplicate
     # pairs, from a matrix or dataframe use thplot2.
     #
     if(length(x1) != length(x2)) {
     cat("\nLengths of vectors not the same\n")
         return()
     }
     temp.x <- remove.na(cbind(x1, x2))
     a1 <- temp.x$x[1:temp.x$n, 1]
     a2 <- temp.x$x[1:temp.x$n, 2]
     ndup <- temp.x$n
     xdif <- abs(a1 - a2)
     xdif[xdif <= 0] <- ifzero
     xbar <- (a1 + a2)/2
     if(is.na(xlow))
         xlow <- min(xbar)
     if(is.na(xhih))
         xhih <- max(xbar)
     if(is.na(yhih))
         yhih <- max(xdif)
     if(yhih == ifzero)
         yhih <- ifzero * 10
     plot(xbar, xdif, xlim = c(xlow, xhih), ylim = c(ifzero, yhih), xlab = "Mean of Duplicates", ylab
          = "Absolute Difference between Duplicates", log = "xy", type = "n", main = main)
     points(xbar, xdif)
     limits <- par("usr")
     ypos <- 10^(limits[3] + (limits[4] - limits[3]) * 0.05)
     text(xhih, ypos, paste("No. of", name, "Duplicates =", ndup), cex = 0.8, adj = 1)
     if(rsd <= 0)
         return()
     calc <- qnorm(1 - (1 - ptile/100)/2) * rsd * 0.014142
     ylcalc <- calc * xlow
     yhcalc <- calc * xhih
     lines(c(xlow, xhih), c(ylcalc, yhcalc))
     ratio <- xbar/xdif
     for(i in 1:ndup) {
         if(ratio[i] <= xlow/ylcalc)
             ratio[i] <- 1
         else ratio[i] <- 0
     }
     nout <- sum(ratio)
     test <- binom.test(nout, ndup, 1 - (ptile/100), "greater")
     ypos <- 10^(limits[4] - (limits[4] - limits[3]) * 0.05)
     label <- paste("\n\nRSD =", rsd, "% (2SD Precision =", 2 * rsd, "%)\nPercentile =", ptile, 
         "%\nDuplicates 'outside' =", nout, "\nProbability =", round(test$p.value, 4))
     text(xlow, ypos, labels = label, cex = 0.8, adj = 0)
     invisible()
}

