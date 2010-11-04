fences <-
function(xx, display = TRUE)
{
     # Function to display the mean, SD, median and MAD, and then the mean +- 2SD,
     # median +- 2MAD, Tukey whisker end fences and the 98th %ile; calculations
     # are made without (top lines) and with (bottom lines) a log10 transformation.
     # 
     # Function may be used with fences.summary to generate fences based on
     # some spatial or contextual framework, i.e. output grouped by a classification
     # factor, that are saved as a file.  The parameter display, when set to FALSE,
     # is used to suppress output to the current device when the function is used
     # with fences.summary.
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # variable, 'x', data must be run through ltdl.fix.df to convert any <dl
     # -ve values to positive half that value, and set zero2na = TRUE if it is
     # required, to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     qntls <- numeric(7)
     table <- numeric(28)
     temp.x <- remove.na(xx)
     x <- temp.x$x[1:temp.x$n]
     nx <- temp.x$n
     nna <- temp.x$nna
     lx <- log10(x)
     qntls <- quantile(x, prob = c(0, 0.02, 0.25, 0.5, 0.75, 0.98, 1))
     table[1] <- nx
     table[2] <- nna
     table[3] <- mean(x)
     table[4] <- sqrt(var(x))
     table[5] <- qntls[4]
     table[6] <- mad(x)
     table[7] <- table[3] + 2 * table[4]
     table[8] <- table[5] + 2 * table[6]
     iqr <- qntls[5] - qntls[3]
     table[9] <- qntls[5] + 1.5 * iqr
     table[25] <- max(x[x < table[9]])
     table[10] <- qntls[6]
     table[11] <- table[3] - 2 * table[4]
     table[12] <- table[5] - 2 * table[6]
     table[13] <- qntls[3] - 1.5 * iqr
     table[26] <- min(x[x > table[13]])
     table[14] <- qntls[2]
     table[15] <- mean(lx)
     table[16] <- sqrt(var(lx))
     table[17] <- log10(qntls[4])
     table[18] <- mad(lx)
     table[19] <- 10^(table[15] + 2 * table[16])
     table[20] <- 10^(table[17] + 2 * table[18])
     liqr <- log10(qntls[5]) - log10(qntls[3])
     table[21] <- 10^(log10(qntls[5]) + 1.5 * liqr)
     table[27] <- max(x[x < table[21]])
     table[22] <- 10^(table[15] - 2 * table[16])
     table[23] <- 10^(table[17] - 2 * table[18])
     table[24] <- 10^(log10(qntls[3]) - 1.5 * liqr)
     table[28] <- min(x[x > table[24]])
     table[3:28] <- signif(table[3:28], 3)
     if(display) {
         cat(" ", deparse(substitute(xx)), "[ All ] :  N =", table[1], "    NAs =",
          table[2], "\t\t2%ile =", table[14], "\t98%ile =", table[10], 
          "\n\t Mean\t  SD\t  Median    MAD\t\t\tMean\2612SD\tMed\2612MAD    Tukey Fences (actual)\n\t",
          table[3], "\t", table[4], "\t  ", table[5], "  ", table[6], "\t+\t", table[7], "\t\t", table[8], 
          "\t      ", table[9], " (", table[25], ")",
          "\n\t\t\t\t\t\t-\t", table[11], "\t\t", table[12], "\t      ", table[13], " (", table[26], ")",
          "\n  Log10\t", table[15], "\t", table[16], "\t  ", table[17], "  ", table[18], "\t+\t", table[19],
          "\t\t", table[20],   "\t      ", table[21], " (", table[27], ")", 
          "\n\t\t\t\t\t\t-\t", table[22], "\t\t", table[23], "\t      ", table[24], " (", table[28], ")\n")
     }
     invisible(table)
}

