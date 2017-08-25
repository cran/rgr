xyplot.tags <-
function (xx, yy, tag = NULL, log = NULL, xlim = NULL, ylim = NULL, 
    xlab = deparse(substitute(xx)), ylab = deparse(substitute(yy)), 
    taglab = NULL, main = "", ...) 
{
    frame()
    if (is.matrix(xx)) {
        if(is.null(taglab)) taglab <- deparse(substitute(yy))
        ylab <- paste("Symmetric coordinate for", dimnames(xx)[[2]][2])
        xlab <- paste("Symmetric coordinate for", dimnames(xx)[[2]][1])
        tag <- yy
        yy <- xx[, 2]
        xx <- xx[, 1]
        log <- NULL
    }
    else if(is.null(taglab)) taglab <- deparse(substitute(tag))
    temp.x <- remove.na(cbind(xx, yy))
    x <- temp.x$x[1:temp.x$n, 1]
    y <- temp.x$x[1:temp.x$n, 2]
    if (main == "") 
        if (taglab == "") 
            banner <- ""
        else banner <- paste("Plot of 'values' for", taglab)
    else banner <- main
    tag[is.na(tag)] <- "+"
    if (is.null(log)) 
        log <- ""
    plot(x, y, log = log, xlim = xlim, ylim = ylim, type = "n", 
        xlab = xlab, ylab = ylab, main = banner, ...)
    text(x, y, tag, ...)
    invisible()
}
