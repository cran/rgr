gx.rma <-
function(xx1, xx2, x1lab = deparse(substitute(xx1)), x2lab = deparse(substitute(xx2)), 
        log = FALSE)
{
     # Function to estimate the coefficients and their SEs of the Reduced Major 
     # Axis, the case of orthogonal regression, based on the methodology in 
     # Miller and Kahn (1962), Statistical Analysis in the Geological Sciences, 
     # pp 204-209.
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # variables, xx1 and xx2, must be run through ltdl.fix.df to convert any <dl
     # -ve values to positive half that value, and set zero2na = TRUE if it is
     # required to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     if(length(xx1) != length(xx2)) stop("Input vectors must be of equal length\n")
     temp.x <- remove.na(cbind(xx1, xx2))
     x1 <- temp.x$x[1:temp.x$n, 1]
     x2 <- temp.x$x[1:temp.x$n, 2]
     cat("\n Reduced Major Axis for", x1lab, "and", x2lab)
     if(log) {
         if(min(min(x1), min(x2)) <= 0)
             stop("Vector(s) contain one or more <= 0 values\n")
         x1 <- log10(x1)
         x2 <- log10(x2)
         cat(" Data have been Log10 transformed\n")
     }
     xlen <- temp.x$n
     x <- cbind(x1, x2)
     xbar <- cbind(mean(x1), mean(x2))
     xvar <- cbind(var(x1), var(x2))
     xsdv <- sqrt(xvar)
     slope <- xsdv[2]/xsdv[1]
     r <- cor(x1, x2)
     if(r < 0) slope <- slope * (-1)
     incpt <- xbar[2] - xbar[1] * slope
     temp <- (1 - r * r)/xlen
     seslp <- slope * sqrt(temp)
     seint <- xsdv[2] * sqrt(temp * (1 + (xbar[1] * xbar[1])/xvar[1]))
     temp <- qt(0.975, xlen - 1)
     cislp <- seslp * temp
     slpll <- slope - cislp
     slpul <- slope + cislp
     ciint <- seint * temp
     intll <- incpt - ciint
     intul <- incpt + ciint
     cat("\n\t\t", x1lab, "\t\t", x2lab, "\n Means =", "\t", signif(xbar[1], 4), 
         "\t\t", signif(xbar[2], 4), "\n SDs =", "\t\t", signif(xsdv[1], 4), 
         "\t\t", signif(xsdv[2], 4), "\n\n Corr =\t\t", round(r, 4), 
         "\n N =\t\t", xlen,  "\n\t\t\t\t   SE\t\t\t95% CLs", "\n Slope =\t", 
         signif(slope, 4), "\t", signif(seslp, 4), "\t  ", signif(slpll, 4), 
         "<->", signif(slpul, 4), "\n Intercept =\t", signif(incpt, 4),
         "\t", signif(seint, 6), "\t  ", signif(intll, 4), "<->",
         signif(intul, 4), "\n\n")
     invisible(list(n = xlen, mean = xbar, sd = xsdv, corr = r, a0 = incpt, 
               a1 = slope, sea0 = seint, sea1 = seslp))
}

