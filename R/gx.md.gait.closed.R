gx.md.gait.closed <-
function(xx, wts = NULL, trim = -1, mvtstart = FALSE, mcdstart = FALSE, 
         main = deparse(substitute(xx)), ifadd = c(0.98,0.95,0.90),
         cexf = 0.6, cex = 0.8, ...)
{
     # A version of gx.md.gait for closed data sets.  An ilr transform is
     # carried out on the data set and the 'gait' performed.  Prior to the 
     # return the covariance matrix and vectors of means and SDs are back
     # transformed to the clr basis, for use in gx.mvalloc.closed.
     #
     # Function to display a Chi^2 plot of Mahalanobis distances for a data 
     # set, with an optional upper tail trim from the distribution; the trim
     # may be defined as the % trimmed, e.g., 0.05 for a 5% MVT, or the
     # number of cases to be trimmed, a number >= 1.
     #
     # Function output may be saved for display of before and after
     # trimming Chi^2 plots with gx.md.plot.
     #
     if(!is.matrix(xx)) stop("  ", deparse(substitute(xx)), " is not a Matrix")
     # Remove any rows containing NAs
     temp.x <- remove.na(xx)
     x <- temp.x$x; n <- temp.x$n; p <- temp.x$m
     # Save variable names
     matnames <- dimnames(xx)
     matnames[[1]] <- c(1:n)
     # Perform ilr transformation
     x.ilr <- ilr(x)
     p.ilr <- p - 1
     #
     if(mcdstart) {
         save <- cov.mcd(x.ilr)
         wts <- rep(0, n)
         wts[save$best] <- 1
         xmean <- save$center
         xcov <- save$cov
         proc <- "mcd"
     }
     else {
         proc <- "wts"
         if(is.null(wts)) {
             wts <- rep(1, n)
             proc <- " "
         }
         save <- cov.wt(x.ilr, wt = wts)
         xmean <- save$center
         xcov <- save$cov
     }
     md <- mahalanobis(x.ilr, xmean, xcov)
     frame()
     if(trim > 0 ) {
         oldpar <- par()
         on.exit(par(oldpar))
         par(mfrow = c(1, 2))
         if(proc == "mcd") wts <- rep(1, n)
         ncore.old <- sum(wts)
         if(trim < 1) {
             # Set up for % MVT trim
             ntrim <- floor(ncore.old * trim)
             ptrim <- trim
             proc <- "mvt"
         }
         else {
             # Set for a trim of 'trim' cases and the
             # special case of a robust "mvt" start
             if(mvtstart) {
                 ncore.old <- n
                 wts <- rep(1, n)
             }
             ntrim <- trim
             ptrim <- -1
             proc <- "gait"
         }
         ncore.new <- ncore.old - ntrim
         if(ncore.new <= 5 * p.ilr) cat("  *** Proceed with Care, ncore <= 5p ***\n")
         if(ncore.new <= 3 * p.ilr) cat("  *** Proceed with Great Care, ncore <= 3p ***\n")
         n1 <- ncore.new + 1
         ordered <- order(md)
         for(i in n1:n) wts[ordered[i]] <- 0
         save <- cov.wt(x.ilr, wt = wts)
         md <- mahalanobis(x.ilr, save$center, save$cov)
         new.core.md <- md[wts[1:n] == 1]
         gx.md.plt0(new.core.md, ncore.new, p.ilr, trim = 0, ptrim = ptrim,
             proc = "", main = "Core Subset", ifadd = ifadd, cexf = cexf,
             cex = cex, ...)
         gx.md.plt0(md, n, p.ilr, trim = n - ncore.new, ptrim = ptrim, 
             proc = proc, main = main, ifadd = ifadd, cexf = cexf, 
             cex = cex, ...)
         par(mfrow = c(1, 1))
     }
     else {
         if(n <= 5 * p.ilr) cat("  *** Proceed with Care, n < 5p ***\n")
         gx.md.plt0(md, n, p.ilr, trim = 0, proc = proc, main = main,
             ifadd = ifadd, cexf = cexf, cex = cex, ...)
         ncore.new <- sum(wts)
         ptrim <- -1
         xsd <- sqrt(diag(xcov))
     }
     temp <- (ncore.new - p.ilr)/(p.ilr * (ncore.new + 1))
     ppm <- 1 - pf(temp * md, p.ilr, ncore.new - p.ilr)
     #
     # Invert ilr covariance matrix for use in gx.mvalloc.closed
     inverted <- ginv(save$cov)
     # Back transform covariances, inverse, means and SDs to clr basis
     V <- orthonorm(p)
     cov.clr <- V %*% save$cov %*% t(V)
     dimnames(cov.clr)[[1]] <- dimnames(cov.clr)[[2]] <- matnames[[2]]
     inverted.clr <- V %*% inverted %*% t(V)
     dimnames(inverted.clr) <- dimnames(cov.clr)
     mean.clr <- as.vector(save$center %*% t(V))
     sd.clr <- sqrt(diag(cov.clr))
     names(mean.clr) <- names(sd.clr) <- matnames[[2]]
     #
     invisible(list(main = main, input = deparse(substitute(xx)),
         matnames = matnames, proc = proc, wts = wts, n = n,
         nc = ncore.new, p = p, ifilr = TRUE, ptrim = ptrim, 
         mean = mean.clr, cov = cov.clr, cov.inv = inverted.clr,
         sd = sd.clr, md = md, ppm = ppm))
}

