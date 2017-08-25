gx.symm.coords.r <-
function (x, log = FALSE, method = "spearman") 
{
    # NOTE: Prior to using this function the data frame/matrix containing the
    # parts, x, must be run through ltdl.fix.df to convert any <dl -ve
    # values to positive half that value, and set zero2na = TRUE if it is
    # required to convert any zero values or other numeric codes representing 
    # blanks to NAs.  Parts in the data frame/matrix, x, should be in the 
    # same units.
    #
    if (dim(x)[2] < 2) stop("data must be of dimension => 2\n") 
    if (any(x < 0)) stop("negative values not allowed\n")
    cat("  ** Are the data/parts all in the same measurement units? **\n")
    xname <- deparse(substitute(x))
    if (is.data.frame(x)) x <- as.matrix(x) 
    #
    xvars <- dimnames(x)[[2]]
    D <- ncol(x)
    work.temp <- matrix(NA, nrow = nrow(x), ncol = (D + 1))
    x.for.r <- matrix(nrow = D, ncol = 2)
    r.sbs <- matrix(NA, nrow = D, ncol = D) 
    #
    work.save <- work.mat <- x
    for(k in 1:(D - 1)) {
        for (j in (k + 1):D) {
            z <- gx.symm.coords(work.mat)
            cor.z <- cor(z[, 1:2], method = method)
            r.sbs[k, j] <- round(cor.z[1, 2], 2)
            if (log) x.for.r <- log(work.mat[, 1:2])
                else {x.for.r <- work.mat[, 1:2]}
            cor.r <- cor(x.for.r, method = method)
            r.sbs[j, k] <- round(cor.r[1, 2], 2)
            work.temp[, 1] <- work.mat[, 1]
            to.end <- work.mat[, 2]
            work.temp[, 2:(D - 1)] <- work.mat[, 3:D]
            work.temp[, D] <- to.end
            work.mat[, 1:D] <- work.temp[, 1:D] 
        }
        to.end <- work.save[, 1]
        work.temp[, 1:(D - 1)] <- work.save[, 2:D]
        work.temp[, D] <- to.end
        work.save[, 1:D] <- work.mat[, 1:D] <- work.temp[, 1:D] 
    }
    if (log) text <- "log transformed"
        else {text <- "untransformed"}
    cat("\n")
    cat(paste("  Symmetric coordinate '", method, "' coefficients and ", 
        text, " '", method, "' coefficients", sep = ""), 
        "\n       Upper and lower triangles, respectively,",
        paste("for ", xname, ", N = ", nrow(x), "\n\n", sep = ""))
    dimnames(r.sbs)[[1]] <- xvars; dimnames(r.sbs)[[2]] <- xvars
    print(r.sbs, na.print = " ")
    cat("\n")
    #
    invisible(r.sbs)
}
