gx.md.print <-
function (xx, pcut = 0.1, file = NULL) 
{
     # Function to display Mahalanobis Distances and membership probabilities
     # together with selected variables from the dataframe the matrix for
     # distance calculations was extracted from.  The resulting table is
     # listed in descending order of Mahalanobis Distances (md) until the
     # probabilities of group membership (ppm) are greater than 'pcut'.  
     # Optionally the table may be exported as a csv file for later use or
     # display with a spreadsheet program.
     #
     # The dataframe from which the matrix passed for Mahalanobis Distance 
     # estimation was generated must be attached so the data for the
     # variables to be appended to the Mahalanobis Distances and
     # probabilities of group membership are available.  In creating the
     # data object to be passed to the function with cbind the 'md's and 
     # 'ppm's MUST be in positions 1 and 2 for the function to sort and
     # display correctly.
     #
     ppm <- xx[, 2]; nrows <- length(ppm[ppm < pcut])
     table.rows <- gx.sort(xx, 1, reverse = TRUE)
     table.rows[, 1:2] <- signif(table.rows[, 1:2], 3)
     dimnames(table.rows)[[2]][1:2] <- c("MD", "p_gm") 
     cat(" ", paste("Table of Mahalanobis Distances where probabilities of ",
         "group membership are <", pcut, sep = ""), "\n")
     print(table.rows[1:nrows, ])
     if(!is.null(file)) write.csv(table.rows, file = file, row.names = FALSE)
     # 
     invisible(list(table.rows = table.rows))
}

