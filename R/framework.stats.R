framework.stats <-
function(xx)
{
     # Function for use with tapply within framework.summary to compute
     # selected percentiles, 95% CI on the median, MAD, IQR estimate of
     # SD, mean, SD and CV%.  The statistical calculations are performed
     # by function gx.stats.
     #
     # Function may be used stand alone to generate a table of summary
     # statistics, e.g., temp <- framework.stats(Cu), however, direct 
     # use of gx.stats is recommended.
     #
     # NOTE: Prior to using this function the data or data frame/matrix
     # containing xx must be run through ltdl.fix or ltdl.fix.df,
     # respectively, to convert any <dl -ve values to positive half that
     # value, and set zero2na = TRUE if it is required to convert any zero
     # values or other numeric codes representing blanks to NAs.
     #
     stats <- gx.stats(xx, display = FALSE, iftell = FALSE)
     table <- numeric(20)
     table[1] <- stats$stats[20]
     table[2] <- length(xx) - stats$stats[20]
     table[3:13] <- stats$stats[c(1, 3, 4, 5, 7, 10, 13, 15, 16, 17, 19)]
     table[14] <- stats$stats[27]
     table[15] <- stats$stats[28]
     table[16] <- stats$stats[21]
     table[17] <- stats$stats[22]
     table[18] <- stats$stats[23]
     table[19] <- stats$stats[25]
     table[14:19] <- signif(table[14:19], 4)
     table[20] <- stats$stats[26]
     invisible(table)
}

