inset.exporter <-
function(x, xlab = deparse(substitute(x)), log = FALSE, xlim = NULL, nclass = NULL, ifnright = TRUE,
     file = NULL, table.cex = 0.7, gtype = "wmf" , ...)
{
     # Wrapper to save the graphics output from a run of inset.
     #
     # NOTE: This functionality is only available under the Windows OS.
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # variable, 'x', data must be run through ltdl.fix.df to convert any <dl
     # -ve values to positive half that value, and set zero2na = TRUE if it is
     # required to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     info <- Sys.info()
     if(!info[1] == "Windows") stop("\n  This function only available under the Windows OS\n")
     ##
     if(is.null(file)) stop("\n  Must supply file name for file, e.g., \"c:\\temp\\file\"\n")
     filename <- paste(file, "_", deparse(substitute(x)) ,sep = "")
     inset(x, xlab = xlab, log = log, xlim = xlim, nclass = nclass, ifnright = ifnright,
         table.cex = table.cex, ...)
     savePlot(file = filename, type = gtype)
     invisible()
}

