\name{thplot1}
\alias{thplot1}
\title{ Display a Thompson-Howarth Plot of Duplicate Measurements }
\description{
Function displays a Thompson-Howarth (1973 & 1978) plot for a set of duplicate measurements to visually inspect them as a part of the QA/QC process.  By inputting a target precision the data may be visually checked to determine if they meet that criterion.  The user is prompted for the location of the two legend items.
}
\usage{
thplot1(x1, x2, xname = "", ifzero = 0.01, xlow = NA, xhih = NA, 
	yhih = NA, rsd = 5, ptile = 95, main = "", ...)
}
\arguments{
  \item{x1}{ a column vector from a matrix or data frame, \code{x1[1], ..., x1[n]}. }
  \item{x2}{ another column vector from a matrix or data frame, \code{x2[1], ..., x2[n]}. \code{x1,x2} must be of identical length, n, where \code{x2} is a duplicate measurement of \code{x1}. }
  \item{xname}{ a title can be displayed with the results, e.g., \cr\code{xname = "Magnetic Susceptibility"}.  If this field is undefined the character string for \code{x1} is used as a default. }
  \item{ifzero}{ as the Thompson-Howarth plot is log-scaled values of zero cannot be displayed, therefore the parameter \code{ifzero} has to be specified.  A suitable choice is a value one order of magnitude lower than the value of the detection limit.  A default value of \code{ifzero = 0.01} units is provided, corresponding to a detection limit of 0.1 units. }
  \item{xlow}{ if is desired to produce plots with consistent scaling this may be achieved by defining \code{xlow}, \code{xhih} and \code{yhih}, the \code{ylow} value is set equal to \code{ifzero}.  Enter an appropriate value of \code{xlow} to ensure all data are displayed on all plots. }
  \item{xhih}{ enter an appropriate value of \code{xhih} to ensure all data are displayed on all plots. }
  \item{yhih}{ enter an appropriate value of \code{yhih} to ensure all data are displayed on all plots. }
  \item{rsd}{ to assist in QA/QC inspection a target precision may be defined as a RSD\%, a default of \code{rsd = 5} is provided. See comments concerning RSD in Details below. }
  \item{ptile}{ defines the confidence interval for a line to be drawn on the plot above which only 100 - ptile\% of the points should plot if the defined target RSD is being met. A default of \code{ptile = 95} is provided. The function counts the number of points falling \sQuote{out of limits} and reports the probability that this number would have fallen \sQuote{out of limits} by chance alone. }
  \item{main}{ a title may be added optionally above the display, e.g., \code{main = "Stanley (2003) Test Data"}. }
  \item{\dots}{ further arguments to be passed to methods.  For example, the size of the axis scale annotation can be change by setting \code{cex.axis}, the size of the axis titles by seetting \code{cex.lab}, and the size of the plot title by setting \code{cex.main}.  For example, if it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%. }  
}
\details{
This function expects the RSD\% as a measure of measurement repeatability (precision), which is more familiar to the current generation of applied geochemists, rather than the precision at the 2 Standard Deviation level.  The necessary calculations to conform with the Thompson and Howarth procedure are made internally.
     
Duplicate pairs containing any \code{NA}s are omitted from the calculations. 

If the data are as a single concatenated vector from a matrix or data frame as \code{x[1], ..., x[n]} followed by \code{x[n+1], ..., x[2n]}, or alternated as \code{x[1]} and \code{x[2]} being a pair through to \code{x[2*i+1]} and \code{x[2*i+2]}, for the \code{i in 1:n} duplicate pairs use function \code{\link{thplot2}}.

The user is prompted for the location of the two legend items added to the plot, the number of duplicate pairs, and whether or not the duplicates have met the RSD\% criterion.  In both instances the user is prompted for the location of left end of the text line, or the top left corner of the text block.
}
\note{ 
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data vector, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Duplicate pairs \code{x1,x2} containing any \code{NA}s are omitted from the calculations.

This script was published by Garrett and Grunsky (2003)
}
\author{ Robert G. Garrett }
\references{ 
Garrett, R.G. & Grunsky, E.C., 2003. S and R functions to display Thompson-Howarth plots. Computers & Geosciences, 29(2):239-242.

Stanley, C.R., 2003. THPLOT.M: A MATLAB function to implement generalized Thompson-Howarth error analysis using replicate data. Computers & Geosciences, 29(2):225-237.

Thompson, M. and Howarth, R.J, 1973. The rapid estimation and control of precision by duplicate determinations. The Analyst, 98(1164):153-160.

Thompson, M. and Howarth, R.J., 1978.  A new approach to the estimation of analytical precision.  Journal of Geochemical Exploration, 9(1):23-30.
}
\seealso{ \code{\link{thplot2}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}


 }
\examples{
## NOTE: the examples below are commented out as thplot1 makes a
## call to the locator function that fails when the examples are run
## during package checking and building
 
## Make the Stanley (2003) test data available
##data(ms.data1)
##attach(ms.data1)

## Display the default plot
##thplot1(MS.1, MS.2, xname = "Magnetic Susceptibility", 
##	main = "Stanley (2003) Test Data")

## Display a Thompson-Howarth plot for a RSD of 7.5\% and a draw the limit
## for a confidence interval of 90\%
##thplot1(MS.1, MS.2, xname = "Magnetic Susceptibility", rsd = 7.5, 
##	ptile = 90, main = "Stanley (2003) Test Data")
     
## Detach test data
##detach(ms.data1)
}
\keyword{ univar }

