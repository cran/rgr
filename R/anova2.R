anova2 <-
function(x, xname = deparse(substitute(x)), log = FALSE, ifalt = FALSE)
{
     # Function to compute a pairs ANOVA for simple duplicates, either
     # sampling and analytical, or analytical.  Duplicate measurements are
     # input as a single vector: x[1], ... x[n] followed by x[n+2], ... x[2n],
     # or alternated as x[1] and x[2] being a pair through to x[2*i+1] and
     # x[2*i+2], for the i in 1:n duplicate pairs, from a matrix or dataframe.
     # The function creates two seperate vectors: x1[1], ... x1[n] and x2[1],
     # ... x2[n] and passes them to function anova1.
     #
     # If the data are as two variables from a matrix or data frame use anova1.
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
     anova1(x1, x2, xname = xname, log = log)
     invisible()
}

