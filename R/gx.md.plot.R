gx.md.plot <-
function(save, main = "", ifadd = c(0.98, 0.95, 0.9), cexf = 0.6, cex = 0.8, 
         ...)
{
     # Function to display Chi^2 plots of Mahalanobis distances saved from
     # either gx.md.gait, gx.mva or gx.robmva; if there are two plots, the
     # left is of the core data and the right is of the total data, both in
     # the context of the robust covariance matrix and means.
     #
     frame()
     if(main == "") banner <- save$input
     else banner <- main
     n <- save$n
     p <- save$p
     if(save$ifilr) p <- p - 1
     nc <- save$nc
     if(nc <= 5 * p) 
         cat("  *** Proceed with Care, Core Size < 5p ***\n")
     if(nc <= 3 * p)
         cat("  *** Proceed with Great Care, Core Size < 3p ***\n")
     md <- save$md
     if(save$proc == "cov") {
         if(length(md) == 0) stop("  No Mahalanobis Distances for ",
             deparse(substitute(save)))
         gx.md.plt0(md, n, p, trim = n - nc, proc = " ", main = banner,
             ifadd = ifadd, cexf = cexf, cex = cex, ...)
     }
     else {
         oldpar <- par()
         on.exit(par(oldpar))
         par(mfrow = c(1, 2))
         wts <- save$wts
         core.md <- md[wts[1:n] == 1]
         gx.md.plt0(core.md, nc, p, trim = 0, proc = save$proc, main = "Core Subset",
             ifadd = ifadd, cexf = cexf, cex = cex, ...)
         gx.md.plt0(md, n, p, trim = n - nc, proc = save$proc, ptrim = save$ptrim,
             main = banner, ifadd = ifadd, cexf = cexf, cex = cex, ...)
     }
     invisible()
}

