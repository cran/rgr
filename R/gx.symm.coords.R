gx.symm.coords <-
function (x)  
{
    # For methodology to compute symmetric coordinates see Kynclova et al.
    # 2017, Mathematical Geosciences, DOI 10.1007/s11004-016-9669-3
    # Script extracted from function 'pivotCoords' in package 
    # robCompositions by Matthias Templ, Karel Hron & Peter Filzmoser.
    #
    # NOTE: Prior to using this function the data frame/matrix containing the
    # parts, xx, must be run through ltdl.fix.df to convert any <dl -ve
    # values to positive half that value, and set zero2na = TRUE if it is
    # required to convert any zero values or other numeric codes representing 
    # blanks to NAs.  Parts in the matrix, xx, must be in the same units, any
    # rows conatining NAs will be removed.
    #
    x <- na.omit(x) 
    D <- ncol(x)
    # 
    Z.av <- matrix(NA, ncol = 2, nrow = nrow(x)) 
    p1 <- sqrt(D - 1 + sqrt(D * (D - 2)))/sqrt(2 * D) 
    p2 <- apply(x[, 3:D], 1, prod) 
    p3 <- (sqrt(D - 2) + sqrt(D))/(sqrt(D - 2) * (D - 1 +  
           sqrt(D * (D - 2)))) 
    p4 <- 1/(D - 1 + sqrt(D * (D - 2))) 
    Z.av[, 1] <- p1 * (log(x[, 1]/(x[, 2]^p4 * p2^p3))) 
    Z.av[, 2] <- p1 * (log(x[, 2]/(x[, 1]^p4 * p2^p3))) 
    return(Z.av) 
}
