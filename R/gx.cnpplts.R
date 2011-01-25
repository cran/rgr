gx.cnpplts <-
function (xlab = " ", log = FALSE, xlim = NULL, main = "", iflgnd = FALSE, 
	...) 
{
     # Function to display up to nine multiple cumulative probability distributions
     # in the same plot.  Optionally an R generated legend may be placed on the
     # plot using the pointing device.
     #
     # NOTE: Prior to using this function the data frames/matrices containing the
     # data to be plotted must be run through ltdl.fix.df to convert any <dl -ve
     # values to positive half that value, and set zero2na = TRUE if it is
     # required to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     # Extract data sets from call, count them - max nine, and determine the
     # data set names
     ListofDataSets <- list(...)
     k <- length(ListofDataSets)
     if(k > 9) stop("Cannot exceed 9 data sets")
     nm <- unlist(as.list(match.call()))[-1]
     start <- 5; stop <- k + 4
     if(!is.null(xlim)) {
         start <- 6
         stop <- k + 5
     }
     names(ListofDataSets) <- nm[start:stop]
     # Determine sizes and ranges for each data set and overall range 
     nxs <- numeric(k)
     ranges <- numeric(k * 2)
     for(j in 1:k) {
         temp <- range(ListofDataSets[[j]], na.rm = TRUE)
         nxs[j] <- length(ListofDataSets[[j]]) 
         ranges[(j*2)-1] <- temp[1]
         ranges[j*2] <- temp[2]
     }
     if(is.null(xlim)) xlim <- range(ranges)
     # Set up for plot, if any <= zero data plot arithmetically
     nxmax <- max(nxs[j])
     alllabels = c("0.001", "0.01", "0.1", "1", "2", "5", "10", "25", "50", "75",
         "90", "95", "98", "99", "99.9", "99.99", "99.999")
     allticks = c(-4.264891, -3.719016, -3.090232, -2.326348, -2.053749, -1.644854, 
         -1.281552, -0.67449, 0, 0.67449, 1.281552, 1.644854, 2.053749, 2.326348, 
         3.090232, 3.719016, 4.264891)
     if(nxmax < 100) clip = 2
     if((nxmax >= 100) && (nxmax <1000)) clip = 2
     if((nxmax >= 1000) && (nxmax < 10000)) clip = 1
     if(nxmax >= 10000) clip = 0
     labticks <- allticks[(1+clip):(17-clip)]
     labels <- alllabels[(1+clip):(17-clip)] 
     setup <- gx.cnpplts.setup()            
     pchs <- setup[[1]]
     symcols <- setup[[2]]
     cex <- setup[[3]]
     cexp <- setup[[4]]
     if(log && (xlim[1] > 0)) logx <- "x"
     else {logx = ""}
     plot(xlim, range(labticks), xlab = xlab, ylab = "% Cumulative Probability",
         log = logx, yaxt = "n", main = main, type = "n") 	
     axis(2, at = labticks, labels = labels, las = 1, cex.axis = 0.8)
     # Display probability plot for each data set
     for(j in 1:k) {
         temp.x <- remove.na(ListofDataSets[[j]])
         nx <- temp.x$n
         x <- sort(temp.x$x[1:nx])
         y <- qnorm(ppoints(nx))
         points(x, y, pch = pchs[j], col = symcols[j], cex = cexp)
         cat("  Data set", j, ";", names(ListofDataSets)[j],
             "; range =", ranges[(j*2)-1], "-", ranges[(j*2)],
             "; nx =", nx, "; pch =", pchs[j], "; col =", symcols[j], "\n")
     }
     # Add legend to the plot if required
     if(iflgnd) legend(locator(1), paste(names(ListofDataSets), ", N = ", nxs, sep = ""),
          pch = pchs, col = symcols, bty = "n", cex = cex)
     invisible()
}

