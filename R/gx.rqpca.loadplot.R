gx.rqpca.loadplot <-
function(save, main = "", crit = 0.3, cex = 0.8, cex.axis = 0.7,
         cex.main = 0.8) 
{
     # Function to plot PCA loadings, based on Peter Filzmoser's loadplot
     # from package StatDA.
     #
     frame()
     oldpar <- par()
     on.exit(par(oldpar))
     par(pty = "m", las = 1)
     if(main == "") banner <- paste("PC loadings >", crit, "for",
         deparse(substitute(save)), "\ndata source:", save$input)
     else banner <- main
     l <- save$rload
     k <- dim(l)[2]
     p <- dim(l)[1]
     # Note, long variable names will result in a cluttered display.
     # use varnames <- substring(varnames,1,3) to extract the first
     # 3 characters and use dimnames(mat.name)[[2]] <- varnames for
     # the input matrix, mat.names, to gx.mva, gx.robmva, etc.
     lnam <- save$matnames[[2]]
     # Plot and annotate axes, etc.
     plot(cbind(c(0, 1, 1, 0, 0), c(-1, -1, 1, 1, -1)), type = "l", 
         axes = FALSE, xlab = "", ylab = "")
     segments(0, 0, 1, 0)
     segments(0, 0.5, 1, 0.5, lty = 2)
     segments(0, -0.5, 1, -0.5, lty = 2)
     tplace1 = -0.3
     mtext("-1", side = 2, at = -1, line = tplace1, cex = cex.axis)
     mtext("-0.5", side = 2, at = -0.5, line = tplace1, cex = cex.axis)
     mtext("0", side = 2, at = 0, line = tplace1, cex = cex.axis)
     mtext("+0.5", side = 2, at = 0.5, line = tplace1, cex = cex.axis)
     mtext("+1", side = 2, at = 1, line = tplace1, cex = cex.axis)
     title(banner, cex.main = cex.main)
     #
     bb <- apply(l^2, 2, sum)/sum(l^2)
     bb1 <- cumsum(bb)
     cumpct <- cumsum(save$econtrib)
     # Plot loadings for each PC, 1:k
     mtext("0%", side = 3, at = 0, line = tplace1, cex = 0.7)
     tplace2 = -0.5
     for (i in 1:k) {
         segments(bb1[i], -1, bb1[i], 1)
         lplot <- abs(l[, i]) > crit
         lsel <- l[lplot, i]
         names(lsel) <- lnam[lplot]
         if (i == 1) {
             mtext(paste(round(cumpct[i]), "%", sep = ""), 
                 side = 3, at = bb1[i], line = tplace1, cex = cex.axis)
             chardist <- bb[1]/(length(lsel) + 1)
             text(seq(from = chardist, by = chardist, length = length(lsel)), 
                 lsel, names(lsel), cex = cex)
             mtext(paste("PC-", round(i), sep = ""), side = 1, at = bb1[i]/2, 
                 line = tplace2, cex = cex.axis)
         }
         else {
             if(length(lsel) >= 1) {
                 mtext(paste(round(cumpct[i]), "%", sep = ""), 
                     side = 3, at = bb1[i], line = tplace1, cex = cex.axis)
                 chardist <- (bb1[i] - bb1[i - 1])/(length(lsel) + 1)
                 text(seq(from = bb1[i - 1] + chardist, by = chardist, 
                     length = length(lsel)), lsel, names(lsel), cex = cex)
                 mtext(paste("PC-", round(i), sep = ""), side = 1, 
                     at = bb[i]/2 + bb1[i - 1], line = tplace2, cex = cex.axis)
             }
         }
     }
     invisible()
}

