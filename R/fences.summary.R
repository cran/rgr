fences.summary <-
function (group, x, file = NULL, units = "ppm") 
{
    if (is.null(file)) 
        stop("Must supply name prefix for output file,", " e.g., \"d:\\\\stuff\\\\soils\"\n", 
            "\t\"_groupname_xname_fences.txt\" will be appended for the full file name\n")
    groupname <- deparse(substitute(group))
    xname <- deparse(substitute(x))
    filename <- paste(file, "_", groupname, "_", xname, "_fences.txt", 
        sep = "")
    cat("  Variable", xname, "subset by", groupname, "- output will be in", 
        filename, "\n")
    sink(filename)
    on.exit(sink())
    framework.fences <- tapply(x, group, fences, units = units, 
        display = FALSE)
    nfences <- length(framework.fences)
    for (i in 1:nfences) {
        gi <- names(framework.fences[i])
        titl <- paste(xname, "[", gi, "]", sep = "")
        table <- unlist(framework.fences[i], use.names = FALSE)
        cat(titl, "\tN =", table[1], "\tNAs =", table[2], 
            "\t\t\t\t2%ile =", table[14], "\t98%ile =", table[10], 
            "\n\tMean\tSD\tMedian\tMAD\t\tMean +\\- 2SD\tMed +\\- 2MAD\tTukey Fences\t(actual)\n", 
            "\t", table[3], "\t", table[4], "\t", table[5], "\t", table[6], "\t+\t",
            table[7], "\t", table[8], "\t", table[9], "\t", table[25], "\n\t\t\t\t\t-\t", 
            table[11], "\t", table[12], "\t", table[13], "\t", table[26], 
            "\nLog10\t", table[15], "\t", table[16], "\t", table[17], "\t", table[18], "\t+\t", 
            table[19], "\t", table[20], "\t", table[21], "\t", table[27], "\n\t\t\t\t\t-\t",
            table[22], "\t", table[23], "\t", table[24], "\t", table[28], 
            "\nLogit\t", table[29], "\t", table[30], "\t", table[31], "\t", table[32], "\t+\t",
            table[33], "\t", table[34], "\t", table[35], "\t", table[39], "\n\t\t\t\t\t-\t", 
            table[36], "\t", table[37], "\t", table[38], "\t", table[40], "\n\n")
    }
    invisible()
}
