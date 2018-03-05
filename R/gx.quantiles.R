gx.quantiles <-
function (x, xname = deparse(substitute(x))) 
{
#   All the 'usual' quantiles you might ever want!
#   Quartiles, quintiles, deciles, and lower and upper extremes.  
#   For other quantiles use 'rgr' function gx.quantile.
    #
    x <- na.omit(x)
    N <- length(x)
    probs <- c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.25, 0.3, 0.4, 0.5,
               0.6, 0.7, 0.75, 0.8, 0.9, 0.95, 0.98, 0.99, 1)
    ptiles <- quantile(x, probs = probs)
    ptiles[2:18] <- signif(ptiles[2:18], 4)
    cat("  Quantiles for:", xname, ", N =", N, 
        "\n\n\t        Min      Q1       Med       Q2       Max\n")
    cat(paste("  Quartiles:   ", ptiles[1], "   ", ptiles[7], "   ", ptiles[10], 
        "   ", ptiles[13], "   ", ptiles[19], "\n\n"))
    cat("  Quintiles:   Min      20      40      60       80       Max\n")
    cat(paste("\t      ", ptiles[1], "   ", ptiles[6], "  ", ptiles[9], "   ", ptiles[11],
        "  ", ptiles[14], "  ", ptiles[19], "\n\n")) 
    cat("  Deciles:     Min      10      20      30       40      50       60        70",
        "       80       90       Max\n")
    cat(paste("\t      ", ptiles[1], "   ", ptiles[5], "  ", ptiles[6], "   ", ptiles[8],
        "  ", ptiles[9], "  ", ptiles[10], "   ", ptiles[11], "   ", ptiles[12], "   ",
        ptiles[14], "   ", ptiles[15], "  ", ptiles[19], "\n\n"))
    cat(paste("  Lower extreme %iles:     Min     1st      2nd     5th     10th     20th\n"))
    cat(paste("\t\t\t  ", ptiles[1], "   ", ptiles[2], "  ", ptiles[3], "   ", ptiles[4], 
        "   ", ptiles[5], "   ", ptiles[6], "\n\n"))
    cat(paste("  Upper extreme %iles:    80th     90th     95th     98th     99th     Max\n"))
    cat(paste("\t\t\t ", ptiles[14], "   ", ptiles[15], "  ", ptiles[16], "   ", ptiles[17], 
        "   ", ptiles[18], "   ", ptiles[19], "\n\n"))
    #
    invisible()
}
