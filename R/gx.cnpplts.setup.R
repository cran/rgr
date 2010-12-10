gx.cnpplts.setup <-
function (display = FALSE) 
{
     # Function to set up, and optionally displaying, the symbols and 
     # colours to be used for the up to nine distributions that may be
     # plotted with gx.cnpplts, the text scaling for the legend 
     # displaying the data (sub)set symbols and population sizes, and
     # the size of the plotting marks.
     #
     # pch: 0 = square, 1 = circle, 2 = triangle, 3 = plus, 4 = X,
     #      5 = diamond, 6 = upside-down triangle, 7 = square with X,
     #      8 = asterisk, 9 = diamond with plus, 10 = circle with plus,
     #      11 = double triangles, 12 = square with plus,
     #      13 = circle with X, 14 = square with upside-down triangle.
     #      Symbols 15 to 18 are solid in the colour specified:
     #      15 = square, 16 = circle, 17 = triangle, 18 = diamond.
     # Col: 0 = none, 1 = black, 2 = red, 3 = green, 4 = dark blue,
     #      5 = turquoise, 6 = pink, 7 = yellow, 8 = gray, 9 = black.
     #
     # See functions display.marks() and display.lty() for examples.
     #
     pchs <- c(0,5,2,6,1,10,12,13,14)
     symcols <- c(1,2,4,3,5,1,6,4,3)
     cex <- 0.8
     cexp <- 0.9
     if(display) {
         frame()
         oldpar <- par()
         on.exit(par(oldpar))
         par(usr = c(0, 3, 0, 3))
         mtext("Symbology for function gx.cnpplts",
               side = 3, line = 2, cex = 2)
         for(i in 1:9) {
             x <- ((i - 1) %% 3) + 0.5
             y <- 2.5 - ((i - 1) %/% 3)
             points(x , y , pch = pchs[i], col = symcols[i], cex = 7)
             text(x, y + 0.35, paste("subset =", i), adj = 0.5)
             text(x, y - 0.35, paste("pchs =", pchs[i], "\nsymcols =",
                 symcols[i]), adj = 0.5)
         }
         abline(h = 0:3, lty = 1)
         abline(v = 0:3, lty = 1)
     }
     invisible(list(pchs = pchs, symcols = symcols, cex = cex, cexp = cexp))   
}

