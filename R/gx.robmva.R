gx.robmva <-
function (xx, proc = "mcd", wts = NULL, main = deparse(substitute(xx))) 
{
    if (!is.matrix(xx)) 
        stop(deparse(substitute(xx)), " is not a Matrix")
    temp.x <- remove.na(xx)
    x <- temp.x$x
    n <- temp.x$n
    p <- temp.x$m
    matnames <- dimnames(xx)
    matnames[[1]] <- c(1:n)
    if (is.null(wts)) {
        if (p > 50) 
            proc <- "mve"
        if (proc == "mve") {
            save <- cov.mve(x, cor = TRUE)
            wts <- array(0, n)
            wts[save$best] <- 1
        }
        else {
            save <- cov.mcd(x, cor = TRUE)
            wts <- array(0, n)
            wts[save$best] <- 1
        }
    }
    else {
        if (length(wts) != n) 
            stop(paste("Length of vector wts, ", length(wts), 
                ", must be equal to the number of individuals, ", 
                n, "\n  Were any individuals with NAs removed by the function?", 
                sep = ""), call. = FALSE)
        proc <- "wts"
        save <- cov.wt(x, wt = wts, cor = TRUE)
    }
    nc <- sum(wts)
    cat("  n = ", n, "\tnc = ", nc, "\tp = ", p, "\t\tnc/p = ", 
        round(nc/p, 2), "\n")
    if (nc < 5 * p) 
        cat("  *** Proceed with Care, Core Size is < 5p ***\n")
    if (nc < 3 * p) 
        cat("  *** Proceed With Great Care, nc = ", nc, ", which is < 3p ***\n")
    temp <- sweep(x, 2, save$center, "-")
    sd <- sqrt(diag(save$cov))
    snd <- sweep(temp, 2, sd, "/")
    b <- svd(save$cor)
    cat("  Eigenvalues:", signif(b$d, 4), "\n")
    sumc <- sum(b$d)
    econtrib <- 100 * (b$d/sumc)
    b1 <- b$v * 0
    diag(b1) <- sqrt(b$d)
    rload <- b$v %*% b1
    rqscore <- snd %*% rload
    pvcontrib <- vcontrib <- numeric(p)
    for (j in 1:p) vcontrib[j] <- var(rqscore[, j])
    sumv <- sum(vcontrib)
    pvcontrib <- (100 * vcontrib)/sumv
    cpvcontrib <- cumsum(pvcontrib)
    rcr <- rload[, ] * 0
    rcr1 <- apply(rload^2, 1, sum)
    rcr <- 100 * sweep(rload^2, 1, rcr1, "/")
    dimnames(rload)[[1]] <- dimnames(rcr)[[1]] <- matnames[[2]]
    if (b$d[p] > 10^-4) {
        md <- mahalanobis(x, save$center, save$cov)
        temp <- (nc - p)/(p * (nc + 1))
        ppm <- 1 - pf(temp * md, p, nc - p)
        epm <- 1 - pchisq(md, p)
    }
    else {
        cat("  Lowest eigenvalue < 10^-4\n", " Trying Moore-Penrose Generalized Inverse\n")
        mpi <- ginv(save$cov)
        md <- mahalanobis(x, save$center, mpi, inverted = TRUE)
        temp <- (nc - p)/(p * (nc + 1))
        ppm <- 1 - pf(temp * md, p, nc - p)
        epm <- 1 - pchisq(md, p)
    }
    invisible(list(main = main, input = deparse(substitute(xx)), 
        proc = proc, n = n, nc = nc, p = p, ifilr = FALSE, matnames = matnames, 
        wts = wts, mean = save$center, cov = save$cov, sd = sd, 
        snd = snd, r = save$cor, eigenvalues = b$d, econtrib = econtrib, 
        eigenvectors = b$v, rload = rload, rcr = rcr, rqscore = rqscore, 
        vcontrib = vcontrib, pvcontrib = pvcontrib, cpvcontrib = cpvcontrib, 
        md = md, ppm = ppm, epm = epm, nr = NULL))
}
