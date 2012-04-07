\name{ad.plot2}
\alias{ad.plot2}
\title{ Plot Results of Analytical Duplicate Analyses, Alternate Input }
\description{
Function to prepare data stored in alternate forms from that expected by function \code{\link{ad.plot1}} for its use. For further details see \code{x} in Arguments below.  The data will be plotted as the percent absolute difference between duplicates relative to their means.
}
\usage{
ad.plot2(x, xname = deparse(substitute(x)), if.order = TRUE, 
	ad.tol = NULL, log = FALSE, ifalt = FALSE, ...)
}
\arguments{
  \item{x}{ a column vector from a matrix or data frame, \code{x[1], ..., x[2*n]}. The default is that the first n members of the vector are the first measurements and the second n members are the duplicate measurements.  If the measurements alternate, i.e. duplicate pair 1 measurement 1 followed by measurement 2, etc., set \code{ifalt = TRUE}. }
  \item{xname}{ a title can be displayed with the plot and results, e.g., \code{xname = "Cu (mg/kg)"}.  If this field is undefined the character string for \code{x} is used as a default. }
  \item{if.order}{ by default the analytical duplicate results are plotted in the order in which they occur in the data file, this usually corresponds to date of analysis in a time-series.  Alternately, setting \code{if.order = FALSE} results in the individual duplicate results being plotted against their means. }
  \item{ad.tol}{ optionally a tolerance level my be provided for the maximum acceptable percent absolute relative difference between duplicates, in which case a red dotted line is added to the plot. }
  \item{log}{ optionally the x-axis of the plot employing duplicate means may be plotted with logarithmic scaling, if so, set \code{log = TRUE}. }
  \item{ifalt}{ set \code{ifalt = TRUE} to accommodate alternating sets of paired observations. }
  \item{\dots}{ any additional arguments to be passed to the \code{plot} function for titling, etc. }
}
\details{
For further details see \code{\link{ad.plot1}}.

If the data are as n duplicate pairs, \code{x1} and \code{x2}, use function \code{\link{ad.plot1}}.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ad.plot1}}, \code{\link{ltdl.fix.df}} }
\examples{
## Make test data available
data(ad.test)
attach(ad.test)

## Plot analytical duplicate analyses as a time-series
ad.plot2(Cu, ifalt = TRUE)

## Plot analytical duplicate analyses versus duplicate means,
## annotating more appropriately, with a 20% maximum tolerance
ad.plot2(Cu, "Cu (mg/kg)", if.order = FALSE, ad.tol = 20, ifalt = TRUE)

## Detach test data
detach(ad.test)
}
\keyword{ univar }
\keyword{ hplot }
