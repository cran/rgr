caplot <-
function (x, y, z, zname = deparse(substitute(z)), caname = deparse(substitute(z)), 
    log = TRUE, ifjit = FALSE, ifrev = FALSE, ngrid = 100, colr = topo.colors(16), 
    xcoord = "Easting", ycoord = "Northing") 
{
    frame()
    oldpar <- par()
    on.exit(par(oldpar))
    par(mfrow = c(2, 2), pty = "m", cex.main = 0.8)
    u <- na.exclude(cbind(x, y, abs(z)))
    xlim <- range(u[, 3])
    cnpplt(u[, 3], xlab = zname, log = TRUE, xlim = xlim, main = "% Cumulative Probability Plot\nOriginal Data", 
        cex.axis = 1, ifshape = TRUE, cex.lab = 0.8)
    if (ifjit) {
        u[, 1] <- jitter(u[, 1], 0.5)
        u[, 2] <- jitter(u[, 2], 0.5)
    }
    zlgnd <- deparse(substitute(z))
    if (log) {
        u[, 3] <- log10(u[, 3])
        zlgnd <- paste("Log10\n", zlgnd)
    }
    xo <- seq(min(u[, 1]), max(u[, 1]), length.out = ngrid)
    yo <- seq(min(u[, 2]), max(u[, 2]), length.out = ngrid)
    new <- interp(u[, 1], u[, 2], u[, 3], xo = xo, yo = yo)
    znew <- na.omit(as.vector(new$z))
    if (log) 
        znew <- 10^znew
    cnpplt(as.vector(znew), xlab = zname, log = TRUE, xlim = xlim, 
        main = "% Cumulative Probability Plot\nGridded Data", 
        cex.axis = 0.8, ifshape = TRUE, cex.lab = 0.8)
    eqscplot(range(new$x), range(new$y), plot = "n", xlab = xcoord, 
        ylab = ycoord, main = caname, pch = 32, cex.lab = 0.8)
    image(new, add = TRUE, col = colr)
    conc <- znew[order(znew)]
    cumarea <- seq(1, length(znew))/length(znew) * 100
    if (!ifrev) 
        conc <- rev(conc)
    plot(conc, cumarea, log = "xy", xlab = zname, ylab = "Cumulative area (%)", 
        main = "Concentration-Area Plot", xlim = xlim, yaxt = "n", 
        pch = 3, cex.lab = 0.8)
    axis(2, at = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", 
        "0.1", "1", "10", "100"), las = 1, cex = 1)
    limits <- par("usr")
    xpos <- 10^(limits[2] - (limits[2] - limits[1]) * 0.05)
    if (ifrev) 
        ypos <- 10^(limits[3] + (limits[4] - limits[3]) * 0.11)
    else ypos <- 10^(limits[4] - (limits[4] - limits[3]) * 0.11)
    text(xpos, ypos, labels = paste("N =", length(conc)), adj = 1, 
        cex = 0.8)
    invisible()
}
