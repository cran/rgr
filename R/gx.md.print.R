gx.md.print <-
function(save, pcut = 0.1, file = NULL) 
{
     ppm <- save$ppm
     nrows <- length(ppm[ppm < pcut])
     table.rows <- cbind(save$matnames[[1]], signif(save$md, 3), signif(ppm, 3))
     dimnames(table.rows)[[2]] <- c("Row ID", "MD", "p_gm")
     table.rows.sorted <- gx.sort(table.rows, 2, reverse = TRUE)
     dimnames(table.rows.sorted)[[2]] <- dimnames(table.rows)[[2]]
     if(is.null(file)) {
         cat(paste("  Table of Mahalanobis Distances where probabilities of ", 
             "group membership (p_gm) are <", pcut, sep = ""), "\n")
         print(table.rows.sorted[1:nrows, ])
         cat("\n")
     }
     else {
         wdname <- getwd()
         if(file == "" | file == " ") 
             filename <- paste(save$input, "_MDs.csv", sep="")
         else filename <- paste(file, ".csv", sep = "") 
         write.csv(table.rows, file = filename, row.names = FALSE)
         filename <- paste(wdname, "/", filename, sep = "")
         cat("  Saved table will be in:\n  ", filename, "\n")
     }
     invisible(table.rows)
}
