syms <-
function(z, zrange = c(NA, NA), p = 1)
{
     # Function to compute the relative diameter, between 0.1 to 1, of a
     # circular symbol for plotting maps and additional variables in x-y
     # or ternary diagrams.  Range may be used, defined via zmin and zmax
     # in calling functions, to truncate the symbols, forcing all lower
     # or higher values to be plotted as a same sized circle.  The rate of
     # change of circle diameter is controlled by p - see syms.pfunc
     # for a function that plots the effect of changes in values of p over
     # the 0 to 1 range over which it is applied.  The final size of the
     # symbol on plotting is controlled by sfact in the calling function.
     #
     dmin <- 0.1
     dmax <- 1
     ddiff <- dmax - dmin
     if(is.na(zrange[1])) zmin <- min(z)
         else zmin <- zrange[1]
     if(is.na(zrange[2])) zmax <- max(z)
         else zmax <- zrange[2]
     zdiff <- zmax - zmin
     z <- ifelse(z < zmin, zmin, z)
     z <- ifelse(z > zmax, zmax, z)
     zdiam <- dmin + ddiff * ((z - zmin)/zdiff)^p
     invisible(zdiam)
}

