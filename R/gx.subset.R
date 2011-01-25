gx.subset <-
function(dfname, subset = TRUE)
{
     # Function, to subset a dataframe on the basis of some criterion or
     # criteria; after Bill Venables, S-News, 10 October 1997.
     #
     subset <- eval(substitute(subset), dfname)
     data.frame(lapply(dfname[subset,  ], function(x)
         if(is.factor(x)) x[, drop = TRUE] else x),
         row.names = row.names(dfname)[subset])
}

