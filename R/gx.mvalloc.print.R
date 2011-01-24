gx.mvalloc.print <-
function(save, ifprint = TRUE, unalloc = TRUE, filename = NULL)
{
     # Function to list posterior allocations and predicted probabilities
     # of group membership from object saved from gx.mvalloc, either to
     # the screen or a file, e.g., "d:\\\\stuff\\\\ecg\\\\ecg_mvalloc.txt".
     #
     kk <- save$kk
     n <- save$n
     cat("  Reference Groups:\n")
     for(k in 1:kk) cat("    ", k, "  ", save$groups[k], "\n")
     cat("  pcrit was set to:", save$pcrit, "\n")
     pgm <- format(save$pgm)
     ix <- format(1:n)
     if(ifprint) {
         cat("  Row    Group  Probabilities of Group Membership:\n")
         for(i in 1:n) {
             if(unalloc & save$xalloc[i] == 0) cat(" ", ix[i], "    ", save$xalloc[i], "  ", pgm[i,  ], "\n")
             if(!unalloc) cat(" ", ix[i], "    ", save$xalloc[i], "  ", pgm[i,  ], "\n")
         }
         cat("\n")
     }
     if(!is.null(filename)) {
         sink(filename)
         cat("I,Group", paste(",pgm[", 1:kk, "]", sep = ""), sep = "")
         for(i in 1:n) cat("\n", i, ",", save$xalloc[i], paste(",", pgm[i,  ], sep = ""), sep = "")
         on.exit(sink())
     }
     invisible()
}

