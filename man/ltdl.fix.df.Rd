\name{ltdl.fix.df}
\alias{ltdl.fix.df}
\title{ Replace Negative Values Representing Less Than Detects in a Data Frame }
\description{
Function to process a matrix or data frame to replace negative values representing less than detects (\code{<}value) with positive half that value.  This permits processing of these effectively categorical data as real numbers and their display on logarithmically scaled axes.  In addition, some software packages replace blank fields that should be interpreted as \code{NA}s, i.e. no information, with zeros.  The facility is provided to replace any zero values with \code{NA}s.  In other instances data files have been built using an integer code, e.g., -9999, to indicate 'no data', i.e. the equivalent of \code{NA}s.  The facility is provided to replace any so coded values with \code{NA}s.  Any factor variables in the input matrix or data frame are passed to the output matrix or data frame.

If a single vector is to be processed, use \code{\link{ltdl.fix}} 

A report of the changes made is displayed on the current device.
}
\usage{
ltdl.fix.df(x, zero2na = FALSE, coded = NA)
}
\arguments{
  \item{x}{ name of the matrix or data frame to be processed. }
  \item{zero2na}{ to replace any zero values with \code{NA}s, set \code{zero2na = TRUE}. }
  \item{coded}{ to replace any numeric coded values, e.g., -9999 with \code{NA}s, set \code{coded = -9999}. }
}
\value{
A matrix or data frame identical to that input but where any negative values have been replaced by half their positive values, and optionally any zero values or numeric coded values have been replaced by \code{NA}s.
}
\note{
Great care needs to be taken when processing data where a large proportion of the data are less than detects (\code{<}value).  In such cases parametric statistics have limited value, and can be missleading.  Records should be kept of variables containing \code{<}values, and the fixed replacement values changed in tables for reports to the appropriate \code{<}values.  Thus, in tables of percentiles the \code{<}value should replace the fixed value computed from absolute(-value)/2.  Various rules have been proposed as to how many less than detects treated in this way can be tolerated before means, variances, etc. become biassed and of little value.  Less than 5\% in a large data set is usually tolerable, with greater than 10\% concern increases, and with greater than 20\% alternate procedures for processing the data should be sought.  For example, the procedures outlined in Helsel (2005).
}
\author{ Robert G. Garrett and David Lorenz }
\references{
Helsel, D.R., 2005. Nondetects and Data Analysis: Statistics for Censored Data. John Wiley & Sons, Ltd., 250 p.
}
\seealso{ \code{\link{ltdl.fix}} }
\examples{
## Replace any missing data coded as -9999 with NAs and any remaining
## negative values representing less than detects with Abs(value)/2
data(fix.test)
fix.test
fix.test.fixed <- ltdl.fix.df(fix.test, coded = -9999)
fix.test.fixed

## As above, and replace any zero values with NAs
fix.test.fixed <- ltdl.fix.df(fix.test, coded = -9999, zero2na = TRUE)
fix.test.fixed

## Clean-up
rm(fix.test.fixed)
}
\keyword{ misc }
 
