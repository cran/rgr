`display.marks` <-
function()
{
     # Function to display pch codes offered on S-News, July 1996, by 
     # Bill Venables, with minor modification by Shawn Boles, 31/7/96.
     #
     frame()
     oldpar <- par()
     on.exit(par(oldpar))
     par(usr = c(0, 5, 0, 5))
     mtext("R plotting marks, e.g., use\npch = 3 to plot plusses",
         side = 1, line = 2, cex = 2)
     for(i in 0:18) {
         x <- 0.5 + (i %% 5)
         y <- 4.5 - (0.5 + (i %/% 5))
         points(x + 0.2, y - 0.2, pch = i, cex = 3)
         text(x - 0.2, y + 0.2, i, cex = 2, adj = 0.5)
         }
     abline(h = 1:5 - 0.5, lty = 1)
     segments(0:5, rep(0.5, 5), 0:5, rep(4.5, 5))
     invisible()
}

