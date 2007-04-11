`framework.summary` <-
function(group, x, file = NULL)
{
     # Function to generate files of framework summary statistics.  The file
     # name prefix should identify the data source, e.g., "kola_c", the group
     # and variable names are appended with "_summary.csv" prior to opening a
     # file for the results in the R working directory; these can be opened or
     # viewed later with MS Excel or other compatible spreadsheet software.
     # Function framework.stats is used to compute the summary statistics. 
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # variable, 'x', data must be run through ltdl.fix.df to convert any <dl
     # -ve values to positive half that value, and set zero2na = TRUE if it is
     # required, to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     # To function correctly the data frame containing the data should be
     # attached prior to running this function, and detached following, i.e. 
     # attach(dfname) and at close, detach(dfname).
     #
     if(is.null(file)) stop("Must supply name prefix for the output file,",
         " e.g., \"kola_c\"\n",
         "\t\"_groupname_xname_summary.csv\" will be appended for the full file name\n")
     groupname <- deparse(substitute(group))
     xname <- deparse(substitute(x))
     filename <- paste(file, "_", groupname, "_", xname, "_summary.csv", sep = "")
     savename <- file.path(getwd(), filename)
     cat("  Variable", xname, "subset by", groupname, "- output will be in", savename, "\n")
     sink(savename)
     cat("Variable,Group,N,NA,Min,2%ile,5%ile,10%ile,25%ile,Median,75%ile,90%ile,95%ile,98%ile,Max,LCI,UCI,MAD,IQSD,Mean,SD,CV%"
         )
     on.exit(sink())
     #
     framework.stats <- tapply(x, group, framework.stats)
     nstats <- length(framework.stats)
     for(i in 1:nstats) {
         gi <- names(framework.stats[i])
         ii <- unlist(framework.stats[i], use.names = FALSE)
         cat("\n", xname, ",", gi, ",", ii[1], ",", ii[2], ",", ii[3], ",", ii[4], ",", ii[5], 
          ",", ii[6],",", ii[7], ",", ii[8], ",", ii[9], ",", ii[10], ",", ii[11], ",", ii[12],
          ",", ii[13], ",", ii[14], ",", ii[15], ",", ii[16], ",", ii[17], ",", ii[18], ",",
          ii[19], ",", ii[20], sep = "")
     }
     invisible()
}

