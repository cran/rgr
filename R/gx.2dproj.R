gx.2dproj <-
function(xx, proc = "sam", log = FALSE, rsnd = FALSE, snd = FALSE,
         range = FALSE, main = "", ...)
{
     # Function to undertake reduced space (k=2) multidimensional scaling
     # ("mds", principal coordinate analysis), preparation of a minimum 
     # spanning tree ("mst"), or computation of a Sammon non-linear map
     # ("sam"), the default.  Requires that Library MASS is attached.  At
     # this time (2010/12/15) no R-script is available for minimum spanning
     # trees in the 'planing' mode, it is not available in R-MASS.
     #
     # Note: Executing a log transform does effect the subsequent computations.
     # Note: A 0-1 range transformation following a SND normalization does not
     # affect the results.  Therefore the software only permits one of the post
     # log transformations, i.e. ether Robust SND, SND or range.  The default
     # is no transformation.  A range transformation with no log transformation,
     # usually results in the lowest stress.  However, SND transformations often
     # lead to informative projections, even though they do not have minimum
     # stress.
     #
     # Note: Not defining main results in the name of the input matrix being
     # used for the plot title.  If a blank title is required, set main = " ", 
     # i.e. a blank between the quotes.
     #
     if(!is.matrix(xx)) stop(deparse(substitute(xx)), " is not a Matrix")
     # Remove any rows containing NAs
     temp.x <- remove.na(xx)
     x <- temp.x$x; n <- temp.x$n
     matnames <- dimnames(xx)
     matnames[[1]] <- c(1:n)
     if(log) {
         x <- log(x)
         cat("  Data have been Log transformed\n")
     }
     if(rsnd) snd <- FALSE
     if(rsnd | snd) range <- FALSE
     if(!rsnd & !snd & !range)
         cat("  You have selected no (r)snd or range transformation\n",
             " Unless the data have been log-ratioed you should consider a transformation\n")
     if(range) x <- rng(x)
     else if(rsnd) {
         x <- scale(x, center = apply(x, 2, median), scale = apply(x, 2, mad))
         cat("  Data have been normalized to Robust SNDs\n")
     }
     else if(snd) {
         x <- scale(x)
         cat("  Data have been normalized to SNDs\n")
     }
     dist.x <- dist(x)
     if(proc == "sam") {
         save <- sammon(dist.x)
         xxx <- save$points[, 1]
         yyy <- save$points[, 2]
         xlabel <- "Sammon Non-Linear Map X Coordinate"
         ylabel <- "Sammon Non-Linear Map Y Coordinate"
     }
     else if(proc == "mds") {
         save <- cmdscale(dist.x, k = 2, eig = TRUE, add = TRUE)
         xxx <- save$points[, 1]
         yyy <- save$points[, 2]
         xlabel <- "Multidimensional Scaling X Coordinate"
         ylabel <- "Multidimensional Scaling Y Coordinate"
         cat("  Eigenvalues:", save$eig, "\n  ac =", save$ac, "\n")
     }
     else {
         cat("  MASS 'mstree' not available for R\n")
#         save <- mstree(x)
#         xxx <- save$x
#         yyy <- save$y
#         xlabel <- "Minimum Spanning Tree X Coordinate"
#         ylabel <- "Minimum Spanning Tree Y Coordinate"
     }
     frame()
     if(main == "")
         banner <- paste("2-D Projection for:", deparse(substitute(xx)))
     else banner <- main
     plot(xxx, yyy, xlab = xlabel, ylab = ylabel, main = banner, ...)
     abline(v = 0, lty = 2)
     abline(h = 0, lty = 2)
     dist.2d <- dist(cbind(xxx, yyy))
     stress <- signif(sum((dist.x - dist.2d)^2)/sum(dist.2d), 5)
     cat(" ", proc, "stress =", stress, "\n")
     usage <- paste(deparse(substitute(xx)), "; proc =", proc, "; log =", log, 
         "; rsnd =", rsnd, "; snd =", snd, "; range =", range)
     #
     invisible(list(main = banner, usage = usage, xlab = xlabel, ylab = ylabel,
         matnames = matnames, x = xxx, y = yyy, stress = stress))
}

