`display.ascii.d` <-
function()
{
     # Displays the ASCII character set by decimal number, was originally
     # show.symbols() offered to S-News.
     #
     frame()
     oldpar <- par()
     on.exit(par(oldpar))
     par(usr = c(0, 17, 0, 17))
     mtext("ASCII character set by decimal number", side = 1, cex = 1.2)
     for(y in 1:16)
         for(x in 1:16) {
             n <- 16 * (y - 1) + x - 1
             points(x + 0.1, y, pch = n, cex = 0.7)
             text(x - 0.3, y + 0.5, n, cex = 0.6, adj = 0)
         }
     lines(c(0.1, 0.1, 16.9, 16.9, 0.1),c(0.5, 17, 17, 0.5, 0.5))
     invisible()
}

