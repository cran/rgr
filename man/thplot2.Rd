\name{thplot2}
\alias{thplot2}
\title{ Display a Thompson-Howarth Plot of Duplicate Measurements, Alternate Input }
\description{
Function to prepare data stored in alternate forms from that expected by function \code{\link{thplot1}} for its use. For further details see 'x' in  Arguments below.  The user is prompted for the location of the two legend items. 
}
\usage{
thplot2(x, xname = deparse(substitute(x)), ifzero = 0.01, 
	xlow = NA, xhih = NA, yhih = NA, rsd = 5, ptile = 95, main = "", 
	ifalt = FALSE, ...)
}
\arguments{
  \item{x}{ a column vector from a matrix or data frame, \code{x[1], ..., x[2*n]}. The default is that the first n members of the vector are the first measurements and the second \code{n} members are the duplicate measurements.  If the measurements alternate, i.e. duplicate pair 1 measurement 1 followed by measurement 2, etc., set \code{ifalt = TRUE}. }
  \item{xname}{ by default the character string for \code{x} is used for the title.  An alternate title can be displayed with \code{xlab = "text string"}, see Examples. }
  \item{ifzero}{ as the Thompson-Howarth plot is log-scaled values of zero cannot be displayed, therefore the parameter \code{ifzero} has to be specified.  A suitable choice is a value one order of magnitude lower than the value of the detection limit.  A default value of \code{ifzero = 0.01} units is provided, corresponding to a detection limit of 0.1 units. }
  \item{xlow}{ if is desired to produce plots with consistent scaling this may be achieved by defining \code{xlow}, \code{xhih} and \code{yhih}, the \code{ylow}, the \code{ylow} value is set equal to \code{ifzero}.  Enter an appropriate value of \code{xlow} to ensure all data are displayed on all plots.  }
  \item{xhih}{ enter an appropriate value of \code{xhih} to ensure all data are displayed on all plots. }
  \item{yhih}{ enter an appropriate value of \code{yhih} to ensure all data are displayed on all plots. }
  \item{rsd}{ to assist in QA/QC inspection a target precision may be defined as a RSD\%, a default of \code{rsd = 5} is provided. See comments concerning RSD in details below. }
  \item{ptile}{ defines the confidence interval for a line to be drawn on the plot above which only 100 - ptile\% of the points should plot if the defined target RSD is being met. A default of \code{ptile = 95} is provided. The function counts the number of points falling \sQuote{out of limits} and reports the probability that this number would have fallen \sQuote{out of limits} by chance alone. }
  \item{main}{ a title may be added optionally above the display, see Example. }
  \item{ifalt}{ set \code{ifalt = TRUE} to accommodate alternating sets of paired observations. } 
  \item{\dots}{ further arguments to be passed to methods.  For example, the size of the axis scale annotation can be change by setting \code{cex.axis}, the size of the axis titles by seetting \code{cex.lab}, and the size of the plot title by setting \code{cex.main}.  For example, if it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%. }  
}
\details{
This function expects the RSD\% as a measure of repeatability (precision), which is more familiar to the current generation of applied geochemists, rather than the precision at the 2 Standard Deviation level.  The necessary calculations to conform with the Thompson and Howarth procedure are made internally.

For further details see \code{\link{thplot1}}.

Duplicate pairs containing any \code{NA}s are omitted from the calculations.

If the data are as n duplicate pairs, x1 and x2, use function \code{\link{thplot1}}.

The user is prompted for the location of the two legend items added to the plot, the number of duplicate pairs, and whether or not the duplicates have met the RSD\% criterion.  In both instances the user is prompted for the location of the left end of the text line, or the top left corner of the text block.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.
}
\author{Robert G. Garrett }
\seealso{ \code{\link{thplot1}}, \code{\link{ltdl.fix.df}} }
\examples{
## NOTE: the examples below are commented out as thplot1 makes a
## call to the locator function that fails when the examples are run
## during package checking and building

## Make test data available
##data(ms.data2)
##attach(ms.data2)
 
## Display the default plot
##thplot2(MS, xname = "Magnetic Susceptibility", 
##	main = "Stanley (2003) Test Data")

## Detach test data
##detach(ms.data2)

## Make test data available
##data(ms.data3)
##attach(ms.data3)

## Display a Thompson-Howarth plot for a RSD of 7.5\% and draw
## the limit for a confidence interval of 90\%
##thplot2(MS, xname = "Magnetic Susceptibility", rsd = 7.5, ptile = 90,
##	main = "Stanley (2003) Test Data", ifalt = TRUE)

## Detach test data
##detach(ms.data3)
} 
\keyword{ univar }
\keyword{ htest }
\keyword{ hplot }

