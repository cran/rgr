gx.summary.mat <-
function(xmat, vars, banner = deparse(substitute(xmat)), log = FALSE)
{
     # Function to generate summary statistics for selected variables in a matrix
     # or data frame.  The variables are defined by their column numbers, e.g., 
     # gx.summary.mat(sind.mat,c(1:6)) or gx.summary.mat(sind,c(4:9)).  The data
     # may be optionally log transformed, in which case the mean and 95% CI on the
     # mean are back transformed to the original units, the SD, CV% and SE remain
     # in log10 units.
     #
     if(!(is.matrix(xmat) | is.data.frame(xmat))) stop(paste("  ", banner,
         "is not a matrix or data frame"))
     nvars <- length(vars)
     if(log) cat("  Data log10 transformed: SD, CV% and SE in log10 units\n")
     cat("  Summary Stats for", banner,"\n\n\t",
         "N NAs - Min Q1 M Q2 Max - MAD IQR_SD - Mean SD CV% - SE 95% CI on Mean\n")
     for(i in 1:nvars) {
         ii <- vars[i]
         if(is.numeric(vars[i])) xname <- dimnames(xmat)[[2]][ii]
         else {xname <- vars[i]}
         table <- gx.summary(xmat[, ii], log = log, iftell = FALSE)
         cat("\n  ", xname, ":\t ", table[1], " ", table[2], " - ", table[3], " ", 
             table[4], " ", table[5], " ", table[6], " ", table[7], " - ", table[8], 
             " ", table[9], " - ", table[10], " ", table[11], " ", table[12], "% - ", 
             table[13], " ", table[14], " <-> ", table[15], "\n", sep = "")
     }
     cat("\n")
     invisible()
}

