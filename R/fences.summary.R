fences.summary <-
function(group, x, file = NULL, units = "ppm")
{
     # Function to generate a file of framework fences to assist in threshold selection.
     # The file name prefix should identify the data source, e.g., "d:\\stuff\\soils",
     # the group name and variable name are appended with "_fences.txt" prior to opening
     # a file for the results; these can be viewed later with Notebook or a viewer.
     # Function fences is used to compute the fences, see there for further details.
     #
     # Note: the logit transformation requires an argument in the range 0 to 1,
     # therefore the input values have to be divided by 100, 10^6, 10^9 or 10^12
     # if the reporting units are percent ("pct"), "ppm" (micro_g/g, mg/kg),
     # "ppb" (pico_g/g, micro_g/kg), or "ppt" (pico_g/kg), respectively.  This is
     # done internally, with the user providing the units, the default is "ppm".
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # variable, 'x', data must be run through ltdl.fix.df to convert any <dl
     # -ve values to positive half that value, and set zero2na = TRUE if it is
     # required, to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     # For the function to perform correctly the data frame containing the data should
     # be attached prior to running this function, and detached following execution,
     # i.e. attach(dfname) and at close, detach(dfname).
     #
     if(is.null(file)) stop("Must supply name prefix for output file,",
         " e.g., \"d:\\\\stuff\\\\soils\"\n",
         "\t\"_groupname_xname_fences.txt\" will be appended for the full file name\n")
     groupname <- deparse(substitute(group))
     xname <- deparse(substitute(x))
     filename <- paste(file, "_", groupname, "_", xname, "_fences.txt", sep = "")
     cat("  Variable", xname, "subset by", groupname, "- output will be in", filename, "\n")
     sink(filename)
     on.exit(sink())
     #
     framework.fences <- tapply(x, group, fences, units = units, display = FALSE)
     nfences <- length(framework.fences)
     for(i in 1:nfences) {
         gi <- names(framework.fences[i])
         table <- unlist(framework.fences[i], use.names = FALSE)
         cat(" ", xname, "[", gi, "] :  N =", table[1], "    NAs =",
          table[2], "\t\t\t\t2%ile =", table[14], "\t98%ile =", table[10], 
          "\n\t Mean\t  SD\t  Median    MAD",
          "\t\tMean\2612SD\tMed\2612MAD\tTukey Fences (actual)\n\t",
          table[3], "\t", table[4], "\t  ", table[5], "\t", table[6], "\t+\t    ", table[7],
          "\t\t    ", table[8], "\t\t    ", table[9], " (", table[25], ")", "\n\t\t\t\t\t-\t    ",
          table[11], "\t\t    ", table[12], "\t\t    ", table[13], " (", table[26], ")",
          "\n  Log10\t", table[15], "\t", table[16], "\t  ", table[17], "\t", table[18], "\t+\t    ",
          table[19], "\t\t    ", table[20],   "\t\t    ", table[21], " (", table[27], ")", 
          "\n\t\t\t\t\t-\t    ", table[22], "\t\t    ", table[23], "\t\t    ", table[24], " (",
          table[28], ")",
          "\n  Logit\t", table[29], "\t", table[30], "\t  ", table[31], "  ", table[32], "\t+\t", 
          table[33], "\t\t", table[34],   "\t      ", table[35], " (", table[39], ")", 
          "\n\t\t\t\t\t\t-\t", table[36], "\t\t", table[37], "\t      ", table[38], " (", table[40],
          ")\n\n")

     }
     invisible()
}

