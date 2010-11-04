dftest <-
function(dfname, x = NULL)
{
     # Function to check for the existence of a data frame and whether it
     # is already attached.  If the data frame is 'legitimate' the variable
     # names are displayed, specifying 'x' displays the length of 'x'.
     #
     dfn <- deparse(substitute(dfname))
     if(exists(dfn)) {
             if(match(dfn, search(), nomatch = 0))
             cat(" ", dfn, "is already attached\n")
         else {
             attach(dfname)
             on.exit(detach(dfname))
         }
         cat("  Names:", names(dfname), "\n")
         if(!is.null(x)) {
             xname <- deparse(substitute(x))
             if(match(xname, names(dfname), nomatch = 0))
             cat(paste("  Length of ", xname, ":", sep = ""), length(x), "\n")
             else cat(" ", xname, "is not present in", dfn, "\n")
         }
     }
     else cat(" ", dfn, "not in search path\n")
     invisible()
}

