gx.mva <-
function (xx, main = deparse(substitute(xx))) 
{
    if (!is.matrix(xx)) 
        stop(deparse(substitute(xx)), " is not a Matrix")
    temp.x <- remove.na(xx)
    x <- temp.x$x
    n <- temp.x$n
    p <- temp.x$m
    matnames <- dimnames(xx)
    matnames[[1]] <- c(1:n)
    wts <- numeric(n)
    wts[1:n] <- 1
    nc <- n
    cat("  n =", n, "\tnc =", n, "\tp =", p, "\t\tnc/p =", round(nc/p, 
        2), "\n")
    if (nc < 5 * p) 
        cat("  *** Proceed with Care, Core Size is < 5p ***\n")
    if (nc < 3 * p) 
        cat("  *** Proceed With Great Care, Core Size = ", n, 
            ", which is < 3p ***\n")
    save <- cov.wt(x, wt = wts, cor = TRUE)
    xmean <- save$center
    xsd <- sqrt(diag(save$cov))
    temp <- sweep(x, 2, xmean, "-")
    snd <- sweep(temp, 2, xsd, "/")
    xsd2 <- sqrt(n) * xsd
    w <- sweep(temp, 2, xsd2, "/")
    wt <- t(as.matrix(w))
    a <- wt %*% as.matrix(w)
    b <- svd(a)
    cat("  Eigenvalues:", signif(b$d, 4), "\n")
    sumc <- sum(b$d)
    econtrib <- 100 * (b$d/sumc)
    rqscore <- w %*% b$v
    pvcontrib <- vcontrib <- numeric(p)
    for (j in 1:p) vcontrib[j] <- var(rqscore[, j])
    sumv <- sum(vcontrib)
    pvcontrib <- (100 * vcontrib)/sumv
    cpvcontrib <- cumsum(pvcontrib)
    b1 <- b$v * 0
    diag(b1) <- sqrt(b$d)
    rload <- b$v %*% b1
    rcr <- rload[, ] * 0
    rcr1 <- apply(rload^2, 1, sum)
    rcr <- 100 * sweep(rload^2, 1, rcr1, "/")
    dimnames(rload)[[1]] <- dimnames(rcr)[[1]] <- matnames[[2]]
    if (b$d[p] > 0.001) {
        md <- mahalanobis(x, save$center, save$cov)
        temp <- (nc - p)/(p * (nc + 1))
        ppm <- 1 - pf(temp * md, p, nc - p)
        epm <- 1 - pchisq(md, p)
    }
    else {
        cat("  Lowest eigenvalue < 10^-4, Mahalanobis distances not computed\n")
        md <- NULL
        ppm <- NULL
        epm <- NULL
    }
    invisible(list(main = main, input = deparse(substitute(xx)), 
        proc = "cov", n = n, nc = nc, p = p, ifilr = FALSE, matnames = matnames, 
        wts = wts, mean = xmean, cov = save$cov, sd = xsd, snd = snd, 
        r = save$cor, eigenvalues = b$d, econtrib = econtrib, 
        eigenvectors = b$v, rload = rload, rcr = rcr, rqscore = rqscore, 
        vcontrib = vcontrib, pvcontrib = pvcontrib, cpvcontrib = cpvcontrib, 
        md = md, ppm = ppm, epm = epm, nr = NULL))
}
