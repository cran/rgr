`ltdl.fix` <-
function(x, zero2na = FALSE, coded = NA)
{
     # Function for pre-processing a vector where -ve values indicating <dl
     # are set to +ve half the value, optionally zeros or other numeric codes,
     # e.g., -9999 may be set to NAs.  All rgr functions remove NAs internally
     # if required.
     #
     n <- length(x)
     nna <- sum(is.na(x))
     cat(" ", n, "records checked,", nna, "NA(s) present")
     if(!is.na(coded)) {
         x[x == coded] <- NA
         ncoded <- sum(is.na(x)) - nna
         cat("\n ", ncoded, "value(s) coded", coded, "set to NA")
     }
     if(zero2na) {
         x[abs(x) < 10^-5] <- NA
         nzero <- sum(is.na(x)) - nna
         cat("\n ", nzero, "zero (abs(x) < 10^-5) record(s) set to NA")
     }
     nfix <- length(x[!is.na(x) & x < 0])
     x[!is.na(x) & x < 0] <- abs(x[!is.na(x) & x < 0])/2
     cat("\n ", nfix, "-ve record(s) set to +ve half the negative value\n")
     invisible(x)
}

