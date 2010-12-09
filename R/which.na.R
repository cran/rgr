which.na <-
function (x) 
{
     # Function to identify the numbers of the rows of a dataframe or
     # matrix containing NAs.  Script from the S-Plus help file on
     # which.na
     whichna <- seq(along = x)[is.na(x)]
     cat("  Rows:", whichna, "\n")
     invisible(whichna)
}

