logit <-
function(pp)
{
     # Function to compute the logit transformation of a variable expressed as
     # a proportion of the total sum of the variables in a composition.  The
     # function expects the proportion to be in the range zero to one.
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # variables 'x' must be run through ltdl.fix.df to convert any <dl -ve
     # values to positive half that value, and set zero2na = TRUE if it is
     # required to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     temp.p <- remove.na(pp)
     p <- temp.p$x[1:temp.p$n]
     for (i in 1:temp.p$n) {
         if((p[i] < 0) | (p[i] > 1))
             stop("The proportion must be in the range 0 to 1")
     }
     z <- log(p) - log(1 - p)
     return(z = z)
}

