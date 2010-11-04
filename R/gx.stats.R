gx.stats <-
function(xx, xlab = deparse(substitute(xx)), display = TRUE)
{
     # Function to generate a 'one-page' univariate summary statistics report.
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # variable, 'xx', data must be run through ltdl.fix.df to convert any <dl
     # -ve values to positive half that value, and set zero2na = TRUE if it is
     # required, to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     rgv <- numeric(7)
     probs <- c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7,
         0.75, 0.8, 0.9, 0.95, 0.98, 0.99, 1)
     qtl <- numeric(19)
     table <- numeric(26)
     temp.x <- remove.na(xx)
     x <- temp.x$x[1:temp.x$n]
     rgv[1] <- temp.x$n
     rgv[2] <- signif(mad(x), 4)
     rgv[4] <- signif(mean(x), 4)
     rgv[5] <- signif(var(x), 4)
     rgv[6] <- signif(sqrt(rgv[5]), 4)
     rgv[7] <- round((100 * rgv[6])/rgv[4], 2)
     qtl <- quantile(x, probs = probs)
     rgv[3] <- signif((qtl[13] - qtl[7]) * 0.7364, 4)
     qtl <- signif(qtl, 4)
     table[1:19] <- qtl[1:19]
     table[20:26] <- rgv[1:7]
     if(display) {
         cat("\n Summary Statistics Display for:", xlab, "\n\n Data Set N =  ", rgv[1], 
             "\n Minimum =     ", qtl[1], "\t\tMaximum =", qtl[19], "\n Median =      ", qtl[10], 
             "\t\tMAD Est =", rgv[2], "\n\t\t\t\tIQR Est =", rgv[3], "\n Mean =        ", rgv[4], 
             "\t\tS.D. =   ", rgv[6], "\n Variance =    ", rgv[5], "\t\tC.V. % = ", rgv[7], "\n")
         cat("\n Table of Percentiles\n\n", "Maximum Value         ", qtl[19], "\n", 
             "99th Percentile       ", qtl[18], "\n", "98th Percentile       ", qtl[17], "\n", 
             "95th Percentile       ", qtl[16], "\n", "90th Percentile       ", qtl[15], "\n", 
             "80th Percentile       ", qtl[14], "\n", "3rd Quartile (75th)   ", qtl[13], "\n", 
             "70th Percentile       ", qtl[12], "\n", "60th Percentile       ", qtl[11], "\n", 
             "Median (50th)         ", qtl[10], "\n", "40th Percentile       ", qtl[9], "\n", 
             "30th Percentile       ", qtl[8], "\n", "1st Quartile (25th)   ", qtl[7], "\n", 
             "20th Percentile       ", qtl[6], "\n", "10th Percentile       ", qtl[5], "\n", 
             " 5th Percentile       ", qtl[4], "\n", " 2nd Percentile       ", qtl[3], "\n", 
             " 1st Percentile       ", qtl[2], "\n", "Minimum Value         ", qtl[1], "\n\n")
    } 
    invisible(list(table = table))
}

