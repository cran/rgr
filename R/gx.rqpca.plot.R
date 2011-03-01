gx.rqpca.plot <-
function(save, v1 = 1, v2 = 2, rplot = TRUE, qplot = TRUE, rowids = NULL, 
         ifrot = TRUE, main = "", cex = 0.7, cex.lab = 0.9, cex.main = 0.9,
         ...)
{
     # Function to plot scores on component v2 vs scores on component v1
     # using the saved output from gx.mva or gx.robmva.  Default is to
     # plot Principal Component 2 vs. Principal Component 1, and data
     # points are plotted as default crosses.  If other than a cross is
     # required, pch is set to the required value.
     #
     # Other component combinations may be plotted by setting v1 and v2
     # appropriately.  The plots are always scaled so that both variables
     # and cases (samples) will plot in the space, this may result in
     # "white space" if either variables or cases are not plotted.
     # Setting ifrot = FALSE causes the unrotated loadings and scores to
     # be plotted from the gx.rotate saved object.
     #
     # Note: Default is rplot = T & qplot = T & rowids = NULL.
     # The following combinations result in the following plots:
     # rplot = T & qplot = T & rowids = NULL, crosses and variable names
     # rplot = T & qplot = F & rowids = NULL, variable names only
     # rplot = F & qplot = T & rowids = NULL, crosses only
     # rplot = F & qplot = T & rowids = F, sample IDs only
     # rplot = F & qplot = T & rowids = T, input matrix row numbers only
     # rplot = T & qplot = T & rowids = F, sample IDs and variable names
     # rplot = T & qplot = T & rowids = T, input matrix row numbers and variable names
     #
     frame()
     if(main == "") banner <- paste("PC bi-plots for",
         deparse(substitute(save)), "\ndata source:", save$input)
     else banner <- main
     nr <- save$nr
     if(is.null(nr)) {
         rload <- save$rload
         rqscore <- save$rqscore
     }
     else {
         if(ifrot) {
             rload <- save$vload
             rqscore <- save$vscore
         }
     else {
         rload <- save$rload
         rqscore <- save$rqscore
         nr <- NULL
         }
     }
     rnames <- save$matnames[[2]]
     qnames <- save$matnames[[1]]
     if(save$proc == "cov") {
         if(is.null(nr)) {
             lv1 <- paste("PC-", as.character(v1), ", ",
                 round(save$pvcontrib[v1],1), "% of total variability", sep = "")
             lv2 <- paste("PC-", as.character(v2), ", ",
                 round(save$pvcontrib[v2],1), "% of total variability", sep = "")
             }
         else {
             lv1 <- paste("Varimax Rotated PC-", as.character(v1), ", ",
                 round(save$pvvcontrib[v1],1), "% of total variability", sep = "")
             lv2 <- paste("Varimax Rotated PC-", as.character(v2), ", ",
                 round(save$pvvcontrib[v2],1), "% of total variability", sep = "")
         }
     }
     else {
         if(is.null(nr)) {
             lv1 <- paste("Robust (", save$proc, ") PC-", as.character(v1), ", ",
                 round(save$pvcontrib[v1],1), "% of total score variability", sep = "")
             lv2 <- paste("Robust (", save$proc, ") PC-", as.character(v2), ", ",
                 round(save$pvcontrib[v2],1), "% of total score variability", sep = "")
         }
         else {
             lv1 <- paste("Robust (", save$proc, ") Varimax Rotated PC-",
                 as.character(v1), "\n", round(save$pvvcontrib[v1],1),
                 "% of total score variability", sep = "") 
             lv2 <- paste("Robust (", save$proc, ") Varimax Rotated PC-",
                 as.character(v2), "\n", round(save$pvvcontrib[v2],1),
                "% of total score variability", sep = "")
         }
     }
     if(rplot & !qplot) {
         x1 <- min(rload[, v1])
         x2 <- max(rload[, v1])
         y1 <- min(rload[, v2])
         y2 <- max(rload[, v2])
     }
     if(!rplot & qplot) {
         x1 <- min(rqscore[, v1])
         x2 <- max(rqscore[, v1])
         y1 <- min(rqscore[, v2])
         y2 <- max(rqscore[, v2])
     }
     if(rplot & qplot) {
         x1 <- min(min(rload[, v1]), min(rqscore[, v1]))
         x2 <- max(max(rload[, v1]), max(rqscore[, v1]))
         y1 <- min(min(rload[, v2]), min(rqscore[, v2]))
         y2 <- max(max(rload[, v2]), max(rqscore[, v2]))
     }
     plot(rqscore[, v1], rqscore[, v2], xlab = lv1, ylab = lv2, 
         xlim = c(x1, x2), ylim = c(y1, y2), type = "n", main = banner,
         cex.main = cex.main, cex.lab = cex.lab, ...)
     if((x1 < 0) & (x2 > 0)) abline(v = 0, lty = 2)
     if((y1 < 0) & (y2 > 0)) abline(h = 0, lty = 2)
     if(rplot) text(rload[, v1], rload[, v2], rnames, cex = cex, ...)
     if(qplot) {
         if(is.null(rowids))
             points(rqscore[, v1], rqscore[, v2], ...)
         else if(rowids) text(rqscore[, v1], rqscore[, v2], cex = cex, ...)
         else text(rqscore[, v1], rqscore[, v2], qnames, cex = cex, ...)
     }
     invisible()
}

