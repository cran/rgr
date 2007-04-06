`framework.stats` <-
function(xx)
{
     # Function for use with tapply within framework.stats to compute
     # selected percentiles, 95% CI on the median, MAD, IQR estimate of
     # SD, mean, SD and CV%.
     #
     # Function may be used stand alone to generate a table of summary
     # statistics, e.g., temp <- framework.stats(Cu)
     #
     # NOTE: Prior to using this function the data or data frame/matrix
     # must be run through ltdl.fix or ltdl.fix.df, respectively,
     # to convert any <dl -ve values to positive half that value, and, if
     # required, to convert any zero values to NAs, zero2na = TRUE.
     #
     probs <- c(0, 0.02, 0.05, 0.1, 0.25, 0.5, 0.75, 0.90, 0.95, 0.98, 1)
     table <- numeric(20)
     temp.x <- remove.na(xx)
     x <- temp.x$x[1:temp.x$n]
     nx <- temp.x$n
     nna <- temp.x$nna
     sortedx <- sort(x)
     j <- qbinom(0.025, nx, 0.5)
     table[1] <- nx
     table[2] <- nna
     table[3:13] <- quantile(sortedx, probs = probs)
     table[14] <- sortedx[j]
     table[15] <- sortedx[nx - j + 1]
     table[16] <- mad(x)
     table[17] <- 0.7613 * (table[8] - table[6])
     table[18] <- mean(x)
     table[19] <- sqrt(var(x))
     table[20] <- (100 * table[19])/table[18]
     table[3:20] <- signif(table[3:20], 4)
     invisible(table)
}

