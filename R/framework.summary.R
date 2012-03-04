framework.summary <-
function (group, x, file = NULL) 
{
    if (is.null(file)) 
        stop("Must supply name prefix for output file,", " e.g., \"d:\\\\stuff\\\\soils\"\n", 
            "\t\"_groupname_xname.csv\" will be appended for the full file name\n")
    groupname <- deparse(substitute(group))
    xname <- deparse(substitute(x))
    filename <- paste(file, "_", groupname, "_", xname, ".csv", 
        sep = "")
    cat("  Variable", xname, "subset by", groupname, "- output will be in", 
        filename, "\n")
    sink(filename)
    cat("Variable,Group,N,NA,Min,2%ile,5%ile,10%ile,25%ile,Median,75%ile,90%ile,95%ile,98%ile,Max,LCI,UCI,MAD,IQSD,Mean,SD,CV%")
    on.exit(sink())
    framework.stats <- tapply(x, group, framework.stats)
    nstats <- length(framework.stats)
    for (i in 1:nstats) {
        gi <- names(framework.stats[i])
        ii <- unlist(framework.stats[i], use.names = FALSE)
        cat("\n", xname, ",", gi, ",", ii[1], ",", ii[2], ",", 
            ii[3], ",", ii[4], ",", ii[5], ",", ii[6], ",", ii[7], 
            ",", ii[8], ",", ii[9], ",", ii[10], ",", ii[11], 
            ",", ii[12], ",", ii[13], ",", ii[14], ",", ii[15], 
            ",", ii[16], ",", ii[17], ",", ii[18], ",", ii[19], 
            ",", ii[20], sep = "")
    }
    invisible()
}
