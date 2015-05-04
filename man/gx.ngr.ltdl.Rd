\name{gx.ngr.ltdl}
\alias{gx.ngr.ltdl}
\title{ Displays a Count of Negative Values and NAs for a Matrix or Data Frame }
\description{
Displays a table of the counts of negative values, indicating less than the detection limit, and NAs to support the preparation of a NGR Report Table of summary statistics.  The table for numerial columns in the matrix or data frame displays the variable name, the number of NAs and the number of negative values, and the number of times the value in coded occurs. 
}
\usage{
gx.ngr.ltdl(xmat, vars, coded = -9999)
}
\arguments{
  \item{xmat}{ name of the matrix or data frame. }
  \item{vars}{ the indices, or names, of the columns of the matrix or data frame for the variables for which counts are to be displayed.  By default, i.e. \code{vars} is undefined, counts for all numeric columns are displayed. }
  \item{coded}{ a coded value, default \code{-9999} whose occurrence will be counted.  In some data sets values greater than the upper limit of quantification are set to such a coded value. }
}
\examples{
## Make test data available
data(fix.test)

## Generate table of counts 
gx.ngr.ltdl(fix.test)
}
\keyword{ univar }

