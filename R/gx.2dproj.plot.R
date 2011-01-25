gx.2dproj.plot <-
function(save, idplot = FALSE, main = "", ...)
{
     # Function to plot the Minimum Spanning Tree ("mst"), the Principal Coordinates
     # ("mds") or Sammon Non-Linear Map ("sam") saved from gx.2dproj with the points
     # identified by matrix row number as a default.  Setting idplot = TRUE results
     # in input matrix row numbers being plotted rather than default symbols, as in
     # gx.2dproj.
     #
     # Note: Not defining main results in the plot title in the saved file being used.
     # If a blank title is required, set main = " ", i.e. a blank between the quotes.
     #
     frame()
     if(main == "")
         banner <- save$main
     else banner <- main
     if(idplot) {
         plot(save$x, save$y, xlab = save$xlab, ylab = save$ylab, type = "n",
             main = banner, ...)
         text(save$x, save$y, save$matnames[[1]], ...)
     }
     else plot(save$x, save$y, xlab = save$xlab, ylab = save$ylab, main = banner, ...)
     abline(v = 0, lty = 2)
     abline(h = 0, lty = 2)
     invisible()
}

