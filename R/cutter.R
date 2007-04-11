`cutter` <-
function(x, cuts) 
{
    # Function to determine in which interval of a set of cut
    # points, cut, a value x falls within or beyond.  The
    # number of intervals is equal to the number of cut points
    # plus 1.  Values of x have to exceed the value of the cut
    # point to be allocated to the higher interval.
    #
    ncuts <- length(cuts)
    xi <- cut(x, breaks = cuts, labels = FALSE, include.left = TRUE)
    xi[x<=cuts[1]] <- 0
    xi[x>=cuts[ncuts]] <- ncuts
    xi <- xi + 1
    invisible(xi)
}

