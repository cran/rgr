gx.sort <-
function(x, col = 1, reverse = FALSE)
{
     # Function to sort a dataframe or matrix, x, by a column number.
     # On exit the function displays the sorted data.  If the function
     # is run as temp <- gx.sort(x, col) the sorted data are not
     # displayed, but retained in temp for subsequent use or display.
     #
     ncol <- length(x[1, ])
     if(col > ncol) stop("Column number must be between 1 and", ncol,
         "\n\n")
     if(reverse) x[rev(order(x[, col])),  ] else x[order(x[, col]),  ]
}

