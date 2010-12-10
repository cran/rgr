gx.pearson <-
function(xx, log = FALSE, ifclr = FALSE, ifwarn = TRUE)
{
     # Function to compute Pearson correlation coefficients and their
     # significance for a matrix, rejecting any rows containing NAs,
     # and display the results to 3 significant figures.
     #
     if(!is.matrix(xx)) stop(deparse(substitute(xx)), " is not a Matrix")
     # Remove any vectors containing NAs
     temp.x <- remove.na(xx)
     x <- temp.x$x
     if(ifclr) log <- FALSE
     if(log) {
         x <- log10(x)
         cat("Data have been Log10 transformed\n")
         }
     else if(ifclr) {
         x <- clr(x)
         cat("Data have been Centred Log-Ratio transformed\n")
         if(ifwarn) cat("** Were the data all in the same measurement units? **\n" )
     }
     #Convert data to SNDs and compute correlation matrix
     z <- scale(x)
     r <- (t(z) %*% z)/(temp.x$n - 1)
     df.t <- temp.x$n - 2
     df.term <- sqrt(df.t)
     for(i in 2:temp.x$m) {
         for(j in 1:i - 1)
             r[i, j] <- pt((abs(r[i, j]) * df.term)/
                        sqrt(1 - r[i, j] * r[i, j]), df.t)
     }
     r <- round(r, 3)
     for(i in 1:temp.x$m) r[i,i] <- NA
     cat("Pearson Correlation Coefficients and their Statistical Significance,",
         "\nupper and lower triangles, respectively,",
         paste("for matrix ", deparse(substitute(xx)), ", N = ", temp.x$n, "\n\n", sep = ""))
     print(r)
     cat("\n")
     invisible()
}

