`cat2list` <-
function(x, a)
{
     # Function from Doug Nytchka, April 28, 1992, that is an integral part
     # of his box-and-whisker plotting function; also used in Tukey boxplot
     # scripts based on Doug Nytchka's original script.
     #
     a <- as.character(a)
     label <- unique(a)
     out <- as.list(1:length(label))
     names(out) <- label
     for(k in 1:length(label)) {
         out[[k]] <- x[label[k] == a]
     }
     out
}

