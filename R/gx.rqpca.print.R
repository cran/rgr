gx.rqpca.print <-
function(save, ifload = TRUE, ifcntrb = FALSE, ifscore = TRUE, file = NULL)
{
     # Function to print the PCA matrices saved from gx.mva, gx.robmva
     # gx.robmva.closed and gx.rotate, and optionally save the scores on
     # the PCs as a '.csv' file in the Working Directory.  The scores are
     # returned and may be saved as an object for easy access by users -
     # rather than having to extract it from a saved object.  Note, '.csv'
     # is appended to the provided file name in the function.
     #
     cat("  PCA matrices for", deparse(substitute(save)), 
         "\n  Source data matrix:", save$input, "\n\n")
     if(ifload) {
         cat("  PC Loadings:\n")
         table.rows <- round(save$rload, 5)
         dimnames(table.rows)[[1]] <- save$matnames[[2]]
         print(table.rows)
         cat("\n")
     }
     if(ifcntrb) {
         cat("  The relative % contributions of the scores of the loadings:\n")
         table.rows <- round(save$rcr, 2)
         dimnames(table.rows)[[1]] <- save$matnames[[2]]
         print(table.rows)
         cat("\n")
     }
     if(ifscore) {
         cat("  Scores on the PCs:\n")
         table.rows <- round(save$rqscore, 4)
         dimnames(table.rows)[[2]] <- paste("PC-",1:save$p,sep="")
         print(table.rows)
         cat("\n")
     }
     if(!is.null(save$nr)) {
         cat("  Following a Varimax rotation:\n")
         if(ifload) {
             table.rows <- round(save$vload, 5)
             dimnames(table.rows)[[1]] <- save$matnames[[2]]
             print(table.rows)
             cat("\n")
         }
         if(ifscore) {
             cat("  Scores on the rotated PCs:\n")
             table.rows <- round(save$vscore, 4)
             dimnames(table.rows)[[2]] <- paste("PC-",1:save$nr,sep="")
             print(table.rows)
             cat("\n")
         }
     }
     if(!is.null(file)) {
         filename <- paste(file, ".csv", sep = "") 
         write.csv(table.rows, file = filename, row.names = TRUE)
     }
     invisible(table.rows)
}

