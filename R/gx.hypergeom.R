gx.hypergeom <-
function(tt, aa, kk, xx)
{
     # Function to compute probabilities that anomalies are non-random 
     # relative to a known target on a traverse.
     #
     # See Stanley, C.R., 2003. Statistical evaluation of anomaly recognition
     # performance. Geochemistry: Exploration, Environment, Analysis. 3(1):3-12.
     #
     # tt = total sites along a traverse; 
     # aa = number of sites that a priori should be anomalous; 
     # kk = total number of >Threshold samples; and
     # xx = number of "aa" sites that are >Threshold.
     #
     pp <- 100 * round(dhyper(xx, aa, tt - aa, kk), 3)
     cat("  Traverse length is", tt, "sites with", aa,
         "sites 'expected' a priori to be anomalous,", 
         "\n  number of >Threshold sites is", kk, "with", xx,
         "coinciding at 'expected' sites.",
         paste("\n  Probability that this is due to 'chance' is ", 
         pp, "%\n",sep = ""))
     invisible()
}

