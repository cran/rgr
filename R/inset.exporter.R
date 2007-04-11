"inset.exporter" <-
function(x, xlab = deparse(substitute(x)), log = FALSE, xlim = NULL, nclass = NULL, ifnright = TRUE,
     file = NULL, gtype = "wmf" , ...)
{
     # Wrapper to save the graphics output from a run of inset in the R working
     # directory.
     #
     # NOTE: Prior to using this function the data frame/matrix containing the
     # variable, 'x', data must be run through ltdl.fix.df to convert any <dl
     # -ve values to positive half that value, and set zero2na = TRUE if it is
     # required, to convert any zero values or other numeric codes representing 
     # blanks to NAs.
     #
     if(is.null(file)) stop("\n  Must supply first part of file name, e.g., \"kola_c\"",
         "\t\"_xname_inset.gtype\" will be appended for the full file name\n")
     filename <- paste(file, "_", deparse(substitute(x)), "_inset", sep ="")
     savename <- file.path(getwd(), filename)
     inset(x, xlab = xlab, log = log, xlim = xlim, nclass = nclass, ifnright = ifnright, ...)
     savePlot(file = savename, type = gtype)
     invisible()
}

