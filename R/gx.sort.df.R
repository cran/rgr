gx.sort.df <-
function(formula, dfname)
{
     # Function to sort a data frame, use +'variable_name' for ascending and
     # -'variable_name' for descending order sorts; sort order is from left
     # to right.
     #
     # Author: Kevin Wright
     # With some ideas from Andy Liaw
     #    http://tolstoy.newcastle.edu.au/R/help/04/07/1076.html
     #
     # If the function is run as temp <- gx.sort.df(~-var1+var4, dfname) the
     # sorted data are not displayed, but retained in temp for subsequent
     # use or display.
     #
     # If dataframe is the formula, then switch formula (formula) and dfname
     if(inherits(dfname, "formula")) {
         f <- dfname
         dfname <- formula
         formula <- f
     }
     if(formula[[1]] != "~") stop("Formula must be one-sided.")
     #
     # Make the formula into character and remove spaces
     formc <- as.character(formula[2])
     formc <- gsub(" ", "", formc)
     # If the first character is not + or -, add +
     if(!is.element(substring(formc, 1, 1), c("+", "-")))
         formc <- paste("+", formc, sep = "")
     # Extract the variables from the formula
     vars <- unlist(strsplit(formc, "[\\+\\-]"))
     vars <- vars[vars != ""]
     # Remove spurious "" terms
     #
     # Build a list of arguments to pass to "order" function
     calllist <- list()
     pos = 1
     # Position of + or -
     for(i in 1:length(vars)) {
         varsign <- substring(formc, pos, pos)
         pos <- pos + 1 + nchar(vars[i])
         if(is.factor(dfname[, vars[i]])) {
             if(varsign == "-")
                 calllist[[i]] <-  - rank(dfname[, vars[i]])
             else calllist[[i]] <- rank(dfname[, vars[i]])
         }
         else {
             if(varsign == "-")
                 calllist[[i]] <-  - dfname[, vars[i]]
             else calllist[[i]] <- dfname[, vars[i]]
         }
     }
     dfname[do.call("order", calllist),  ]
}

