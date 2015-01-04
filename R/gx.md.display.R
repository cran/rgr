gx.md.display <-
function (xx, pcut = 0.1, file = NULL) 
{
     # Function to display Mahalanobis Distances and membership probabilities
     # together with selected variables from the dataframe or matrix used for
     # Mahalanobis distance estimations.  The data are sorted in order of
     # increasing probability of group membership and only those 'samples'
     # probabilities less that pcut are displayed.
     # Alternately, the entire table may be exported as a .csv file for later
     # use or display with a spreadsheet program.  If file is set to "" or
     # " " a default file name is generated.
     #
     # The dataframe from which the matrix passed for Mahalanobis Distance 
     # estimation was generated must be attached so the data for the
     # variables to be appended to the Mahalanobis Distances and
     # probabilities of group membership are available.  In creating the
     # data object to be passed to the function with cbind the 'MD_s' and 
     # 'ppm_s' from the saved object MUST be in positions 1 and 2 for the
     # function to sort and display correctly.
     #
     dimnames(xx)[[2]][1:2] <- c("MD", "p_gm") 
     if(is.null(file)) {
         ppm <- xx[, 2]; nrows <- length(ppm[ppm < pcut])
         table.rows <- gx.sort(xx, 1, reverse = TRUE)
         table.rows[, 1:2] <- signif(table.rows[, 1:2], 3)
         cat(paste("  Table of Mahalanobis Distances and probabilities of ", 
             "group membership (p_gm) are <", pcut, sep = ""), "\n")
         table.rows <- table.rows[1:nrows, ]
         print(table.rows)
         cat("\n")
     }
     else {
         table.rows <- xx
         wdname <- getwd()
         if(file == "" | file == " ") filename <- "MDs_&_variables.csv"
         else filename <- paste(file, ".csv", sep = "") 
         write.csv(table.rows, file = filename, row.names = FALSE)
         filename <- paste(wdname, "/", filename, sep = "")
         cat("  Saved table will be in:\n  ", filename, "\n")
     }
     # 
     invisible(table.rows)
}
