gx.md.display <-
function (xx, pcut = 0.1, file = NULL) 
{
    ppm <- xx[, 2]
    nrows <- length(ppm[ppm < pcut])
    table.rows <- gx.sort(xx, 1, reverse = TRUE)
    table.rows[, 1:2] <- signif(table.rows[, 1:2], 3)
    dimnames(table.rows)[[2]][1:2] <- c("MD", "p_gm")
    cat(" ", paste("Table of Mahalanobis Distances where probabilities of ", 
        "group membership are <", pcut, sep = ""), "\n")
    print(table.rows[1:nrows, ])
    cat("\n")
    if (!is.null(file)) {
        filename <- paste(file, ".csv", sep = "")
        write.csv(table.rows, file = filename, row.names = FALSE)
    }
    invisible(table.rows)
}
