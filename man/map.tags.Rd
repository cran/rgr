\name{map.tags}
\alias{map.tags}
\title{ Plot a Map of Posted Values }
\description{ Displays a simple map where the data are represented by the \sQuote{written} values of the data at their spatial locations. }
\usage{
map.tags(xx, yy, tag, xlab = "Easting", ylab = "Northing",
	taglab = deparse(substitute(tag)), main = "", tol = 0.04, ...)
}
\arguments{
  \item{xx}{ name of the x-axis spatial coordinate, the eastings. }
  \item{yy}{ name of the y-axis spatial coordinate, the northings. }
  \item{tag}{ name of the variable to be plotted as a map. }
  \item{xlab}{ a title for the x-axis, defaults to \code{Easting}. }
  \item{ylab}{ a title for the y-axis, defaults to \code{Northing}. }
  \item{taglab}{ by default, \code{taglab = deparse(substitute(tag))}, a map title is generated by appending the input variable name text string to \code{"Map of Values for "}. Alternative titles may be generated, see Details below. }
  \item{main}{ an alternative map title, see Details below.}
  \item{tol}{ a parameter used to ensure the area included within the neatline around the map is larger than the distribution of the points so that the plotted symbols fall within the neatline.  By default \code{tol = 0.04}, if more clearance is required increase the value of \code{tol}. }
  \item{\dots}{ further arguments to be passed to methods.  For example, if smaller plotting characters are required, specify \code{cex = 0.8}; or if some colour other than black is required for the plotting characters, specify \code{col = 2} to obtain red (see \code{\link{display.lty}} for the default colour palette).  If it is required to make the map title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%. }
}
\details{
If \code{taglab} and \code{main} are undefined a default a map title is generated by appending the input variable name text string to \code{"Map of Values for "}.  If no map title is required set \code{taglab = ""}, or if an alternative to the variable name \code{taglab} is required it may be specified, \code{taglab = "Alternative"}.  If some user defined map title is required it should be defined in \code{main}, e.g. \code{main = "Map Title Text"}, in which instance \code{taglab} is ignored.

If a map of sample numbers, \sQuote{IDs}, is required and they are not explicitly in the data frame, a map of data frame row numbers may be displayed by specifying \code{dimnames(dfname)[[1]]} as the value of \code{tags}.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any data vectors including \code{NA}s for spatial coordinates are removed prior to displaying the map, thus those \sQuote{sites} are not plotted.  However, where coordinates are present any \code{NA}s in the variable to be plotted are replaced with a \sQuote{+} sign to indicate sites with \sQuote{missing data}.

In some R installations the generation of multi-panel displays and the use of function eqscplot from package MASS causes warning messages related to graphics parameters to be displayed on the current device. These may be suppressed by entering \code{options(warn = -1)} on the R command line, or that line may be included in a \sQuote{first} function prepared by the user that loads the \sQuote{rgr} package, etc.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{display.lty}} }
\examples{
## Make test data available
data(sind)
attach(sind)

## Plot a sample site number map
map.tags(E, N, ID)

## Plot a sample site number map with smaller numbers
## and a wider internal map margin
map.tags(E, N, ID, cex = 0.8, tol = 0.06)

## Plot the data frame row numbers rather than the sample
## numbers
map.tags(E, N, dimnames(sind)[[1]], cex = 0.8, tol = 0.06)

## Plot the values for Zn in red, providing enough internal
## map margin so the values do not overprint the map neat-line
map.tags(E, N, Zn, cex = 0.8, tol = 0.1, col = 2)

## Plot as above but with an informative title spread over
## two lines and with a slightly smaller font
map.tags(E, N, Zn, cex = 0.8, tol = 0.1, col = 2, main =
	"Howarth & Sinding-Larsen\nStream Sediment Zn Data",
	cex.main = 0.9) 

## Detach test data
detach(sind) 
}
\keyword{ hplot }

