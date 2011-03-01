gx.md.print <-
function(save, pcut = 0.1, file = NULL) 
{
     cat("  Mahalanobis distances for", deparse(substitute(save)), 
         "\n  Source data matrix:", save$input, "\n\n")
     ppm <- save$ppm
     nrows <- length(ppm[ppm < pcut])
     table.rows <- gx.sort(cbind(save$matnames[[1]], save$md, ppm), 2, reverse = TRUE)
     dimnames(table.rows)[[2]] <- c("Row ID", "MD", "p_gm")
     table.rows[, 2:3] <- signif(table.rows[, 2:3], 3)
     cat(paste("  Table of Mahalanobis Distances where probabilities of ", 
         "group membership are <", pcut, sep = ""), "\n")
     print(table.rows[1:nrows, ])
     cat("\n")
     if(!is.null(file)) {
         filename <- paste(file, ".csv", sep = "") 
         write.csv(table.rows, file = filename, row.names = FALSE)
     }
     invisible(table.rows)
}

