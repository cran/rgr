gx.summary1 <-
function(xx, xname = deparse(substitute(xx)), log = FALSE)
{
     # Function to display summary statistics for a selected variable.  The data
     # may be optionally log transformed, in which case the mean and 95% CI on the
     # mean are back transformed to the original units, the SD, CV% and SE remain
     # in log10 units.
     #
     if(log) cat("  Data log10 transformed: SD, CV% and SE in log10 units, estimates",
         "for the\n  Mean and 95% CIs have been back-transformed\n")
     table <- gx.summary(xx, log = log, iftell = FALSE)
     cat("\n  Summary Stats for:", xname, "\n\n",
         " N  NAs - Min Q1 M Q3 Max - MAD IQR_SD - Mean SD CV% - SE & 95% CLs on Mean\n\n")
     cat("  ", table[1], " ", table[2], " - ", table[3], " ", table[4], " ",
         table[5], " ", table[6], " ", table[7], " - ", table[8], " ", table[9], 
         " - ", table[10], " ", table[11], " ", table[12], "% - ", table[13], 
         " ", table[14], " <-> ", table[15], "\n\n", sep = "")
     invisible()
}

