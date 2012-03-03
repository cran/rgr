gx.rotate <-
function (save, nrot = 2) 
{
    if (nrot > save$p) 
        nrot <- save$p
    amat <- matrix(nrow = save$n, ncol = nrot)
    amat <- save$rload[, 1:nrot]
    kaiser <- varimax(amat, normalize = FALSE)
    vscore <- save$snd %*% kaiser$loadings %*% solve(t(kaiser$loadings) %*% 
        kaiser$loadings)
    pvcontrib <- vcontrib <- numeric(nrot)
    for (j in 1:nrot) vcontrib[j] <- var(vscore[, j])
    pvcontrib <- (100 * vcontrib)/sum(vcontrib)
    cpvcontrib <- cumsum(pvcontrib)
    invisible(list(main = save$main, input = save$input, proc = save$proc, 
        n = save$n, nc = save$nc, p = save$p, matnames = save$matnames, 
        wts = save$wts, mean = save$mean, cov = save$cov, sd = save$sd, 
        snd = save$snd, r = save$r, eigenvalues = save$eigenvalues, 
        econtrib = save$econtrib, eigenvectors = save$eigenvectors, 
        rload = save$rload, rcr = save$rcr, rqscore = save$rqscore, 
        vcontrib = save$vcontrib, pvcontrib = save$pvcontrib, 
        cpvcontrib = save$cpvcontrib, md = save$md, ppm = save$ppm, 
        epm = save$epm, nr = nrot, vload = kaiser$loadings, vscore = vscore, 
        vvcontrib = vcontrib, pvvcontrib = pvcontrib, cpvvcontrib = cpvcontrib))
}
