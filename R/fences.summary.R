`fences.summary` <-
function(group, x, file = NULL)
{
     # Function to generate a file of framework or subsetfences to assist in
     # threshold selection.  The file name prefix should identify the data source,
     # e.g.,"kola_c", the framework and variable names are appended with
     # "_fences.txt_" prior to opening a file for the results in the R working
     # directory; these can be viewed and cosmetically edited later with a viewer
     # or ASCII editor.  Function fences is used to generate the fence statistical
     # estimates.
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # variable, 'x', data must be run through ltdl.fix.df to convert any <dl
     # -ve values to positive half that value, and set zero2na = TRUE if it is
     # required, to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     # For the function to perform correctly the dataframe containing the data
     # should be attached prior to running this function, and detached following
     # execution, i.e. attach(dfname) and at close, detach(dfname).
     #
     if(is.null(file)) stop("Must supply name prefix for the output file,",
         " e.g., \"kola_c\"\n",
         "\t\"_groupname_xname_fences.txt\" will be appended for the full file name\n")
     groupname <- deparse(substitute(group))
     xname <- deparse(substitute(x))
     filename <- paste(file, "_", groupname, "_", xname, "_fences.txt", sep = "")
     savename <- file.path(getwd(), filename)
     cat("  Variable", xname, "subset by", groupname, "- output will be in", savename, "\n")
     sink(savename)
     on.exit(sink())
     #
     framework.fences <- tapply(x, group, fences, display = FALSE)
     nfences <- length(framework.fences)
     for(i in 1:nfences) {
         gi <- names(framework.fences[i])
         table <- unlist(framework.fences[i], use.names = FALSE)
         cat(" ", xname, "[", gi, "] :  N =", table[1], "    NAs =",
          table[2], "\t\t\t\t2%ile =", table[14], "\t98%ile =", table[10], 
          "\n\t Mean\t  SD\t  Median    MAD",
          "\t\tMean\2612SD\tMed\2612MAD\tTukey Fences (actual)\n\t",
          table[3], "\t", table[4], "\t  ", table[5], "\t", table[6], "\t+\t    ", table[7], "\t\t    ",
          table[8], "\t\t    ", table[9], " (", table[25], ")",
          "\n\t\t\t\t\t-\t    ", table[11], "\t\t    ", table[12], "\t\t    ", table[13], " (", table[26], ")",
          "\n  Log10\t", table[15], "\t", table[16], "\t  ", table[17], "\t", table[18], "\t+\t    ",
          table[19], "\t\t    ", table[20],   "\t\t    ", table[21], " (", table[27], ")", 
          "\n\t\t\t\t\t-\t    ", table[22], "\t\t    ", table[23], "\t\t    ", table[24], " (",
          table[28], ")\n\n")
     }
     invisible()
}

