gx.rqpca.print <-
function(save, ifload = TRUE, ifcntrb = FALSE, ifscore = TRUE, file = NULL)
{
     # Function to print the PCA matrices saved from gx.mva, gx.mva.closed,
     # gx.robmva, gx.robmva.closed and gx.rotate, and optionally save the
     # scores on the PCs as a '.csv' file in the Working Directory.  The 
     # last table generated, by default the scores, is returned and may be
     # saved as an object for easy access by users, rather than having to 
     # extract the scores from a saved object.
     # If ifload and ifcntrb and ifscore are all FALSE, and file contains
     # text, that text will be used for the name of a '.csv' file of the
     # PC or rotated PC scores.  In the foregoing instance if file is not
     # defined the function fails with "object 'table.rows' not found".
     # Note, '.csv' is appended to the provided file name in the function.
     #
     cat("  PCA matrices for", deparse(substitute(save)), 
         "\n  Source data matrix:", save$input, "\n\n")
     if(ifload) {
         cat("  PC Loadings:\n")
         table.rows <- round(save$rload, 5)
         dimnames(table.rows)[[1]] <- save$matnames[[2]]
         dimnames(table.rows)[[2]] <- paste("PC-", 1:save$p, sep="")
         print(table.rows)
         cat("\n")
     }
     if(ifcntrb) {
         cat("  The relative % contributions of the scores of the loadings:\n")
         table.rows <- round(save$rcr, 2)
         dimnames(table.rows)[[1]] <- save$matnames[[2]]
         dimnames(table.rows)[[2]] <- paste("PC-", 1:save$p, sep="")
         print(table.rows)
         cat("\n  The cumulative relative % contributions of the PCs:\n")
         temp.table <- matrix(nrow=save$p, ncol=save$p)
         for (i in 1:save$p) {
             cumulation <- cumsum(save$rcr[i, ])
             temp.table[i, ] <- round(cumulation, 2)
         }
         dimnames(temp.table)[[1]] <- save$matnames[[2]]
         dimnames(temp.table)[[2]] <- paste("PC-", 1:save$p, sep="")
         print(temp.table)
         cat("\n")
     }
     if(ifscore) {
         cat("  Scores on the PCs:\n")
         table.rows <- round(save$rqscore, 4)
         dimnames(table.rows)[[2]] <- paste("PC-", 1:save$p, sep="")
         print(table.rows)
         cat("\n")
     }
     if(!is.null(save$nr)) {
         cat("  Following a Varimax rotation:\n")
         if(ifload) {
             table.rows <- round(save$vload, 5)
             dimnames(table.rows)[[1]] <- save$matnames[[2]]
             dimnames(table.rows)[[2]] <- paste("PC-", 1:save$p, sep="")
             print(table.rows)
             cat("\n")
         }
         if(ifscore) {
             cat("  Scores on the rotated PCs:\n")
             table.rows <- round(save$vscore, 4)
             dimnames(table.rows)[[2]] <- paste("PC-", 1:save$nr, sep="")
             print(table.rows)
             cat("\n")
         }
     }
     if(!is.null(file)) {
         wdname <- getwd()
         filename <- paste(file, ".csv", sep = "") 
         if(!ifload&!ifcntrb&!ifscore) {
             if(is.null(save$nr)) {
                 table.rows <- round(save$rqscore, 4)
                 dimnames(table.rows)[[2]] <- paste("PC-", 1:save$p, sep="")
                 cat("  PC scores will be saved in:")
             }
             else {
                 table.rows <- round(save$vscore, 4)
                 dimnames(table.rows)[[2]] <- paste("PC-", 1:save$nr, sep="")
                 cat("  Rotated PC scores will be saved in:")
             }      
         }
         write.csv(table.rows, file = filename, row.names = TRUE)
         filename <- paste(wdname, "/", filename, sep = "")
         cat("\n  ", filename, "\n")
     }
     invisible(table.rows)
}
