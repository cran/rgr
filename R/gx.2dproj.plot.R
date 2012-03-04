gx.2dproj.plot <-
function (save, idplot = FALSE, main = "", ...) 
{
    frame()
    if (main == "") 
        banner <- save$main
    else banner <- main
    if (idplot) {
        plot(save$x, save$y, xlab = save$xlab, ylab = save$ylab, 
            type = "n", main = banner, ...)
        text(save$x, save$y, save$matnames[[1]], ...)
    }
    else plot(save$x, save$y, xlab = save$xlab, ylab = save$ylab, 
        main = banner, ...)
    abline(v = 0, lty = 2)
    abline(h = 0, lty = 2)
    invisible()
}
