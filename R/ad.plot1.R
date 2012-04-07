ad.plot1 <-
function (x1, x2, xname = deparse(substitute(x1)), if.order = TRUE, 
    ad.tol = NULL, log = FALSE, ...) 
{
    if (length(x1) != length(x2)) 
        stop("The lengths of the vectors are not the same\n")
    temp.x <- remove.na(cbind(x1, x2))
    a1 <- temp.x$x[1:temp.x$n, 1]
    a2 <- temp.x$x[1:temp.x$n, 2]
    n <- temp.x$n
    means <- (a1 + a2)/2
    diffs <- abs(a1 - a2)
    error.sd <- sqrt(sum(diffs * diffs)/(2 * n))
    mean <- mean(means)
    rsd <- round(100 * error.sd/mean, 2)
    mads <- numeric(n)
    for (i in 1:n) {
        mads[i] <- mad(c(a1[i], a2[i]))
    }
    rmed <- median(means)
    rmad <- median(mads)
    rrsd <- round(100 * rmad/rmed, 2)
    cat(" Analytical duplicates for:", xname, "\n This batch:\t N =", 
        n, "\t  Mean =", signif(mean, 3), "\t SD =", signif(error.sd, 
            3), "\t RSD% =", rsd, "\n\t\t\t\tMedian =", signif(rmed, 
            3), "\tMAD =", signif(rmad, 3), "\trRSD% =", rrsd, 
        "\n")
    pdiff <- 100 * diffs/means
    ymax <- max(ad.tol, max(pdiff))
    if (if.order) {
        par(pty = "m")
        plot(seq(1:n), pdiff, xlab = paste("Ordered determinations of", 
            xname), ylab = "Difference between duplicates relative to their means, %", 
            ylim = c(0, ymax), ...)
        par(pty = "s")
    }
    else {
        log.plot <- ""
        if (log) 
            log.plot <- "x"
        plot(means, pdiff, xlab = paste("Mean of duplicates for", 
            xname), ylab = "Difference between duplicates relative to their means, %", 
            log = log.plot, ylim = c(0, ymax), ...)
    }
    if (!is.null(ad.tol)) 
        abline(h = ad.tol, col = 2, lty = 2)
    invisible()
}
