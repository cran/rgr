gx.summary.groups <-
function(group, x, xname = deparse(substitute(x)), log = FALSE)
{
     # Function to generate summary statistics of data by subset.  The data may
     # be optionally log transformed, in which case the mean and 95% CI on the
     # mean are back transformed to the original units, the SD, CV% and SE remain
     # in log10 units.
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
     groupname <- deparse(substitute(group))
     group.stats <- tapply(x, group, gx.summary, log = log)
     if(log) cat("  Data log10 transformed: SD, CV% and SE in log10 units\n")
     cat("  Summary Stats for", xname, "subset by", groupname, "\n\t",
         "N NA - Min Q1 M Q2 Max - MAD IQR_SD - Mean SD CV% - SE 95% CI on Mean\n")
     nstats <- length(group.stats)
     for(i in 1:nstats) {
         gi <- names(group.stats[i])
         ii <- unlist(group.stats[i], use.names = FALSE)
         cat("\n  ", gi, ":\t ", ii[1], " ", ii[2], " - ", ii[3], " ", ii[4], " ",
         ii[5], " ", ii[6], " ", ii[7], " - ", ii[8], " ", ii[9], " - ", ii[10],
         " ", ii[11], " ", ii[12], "% - ", ii[13], " ", ii[14], " <-> ", ii[15],
         "\n", sep = "")
     }
     cat("\n")
     invisible()
}

