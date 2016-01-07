\name{gx.mf}
\alias{gx.mf}
\title{ Prepare a Concentration-Number (C-N) Plot }
\description{
Display a log-log Concentration-Number (C-N) Plot to inspect a data set for multifractality.
}
\usage{
gx.mf(xx, xlab = deparse(substitute(xx)), 
        ylab = "Cumulative Percentage of Data", main = "Multifractality Plot",
        ifrev = FALSE, xlim = range(xx, na.rm = TRUE), ...)
}
\arguments{
  \item{xx}{ name of the variable to be plotted. }
  \item{xlab}{ by default the character string for \code{xx} is used for the x-axis plot titles.  An alternate title can be displayed with \code{xlab = "text string"}, see Examples. }
  \item{ylab}{ by default the y-axia is labelled \sQuote{Cumulative Percentage of Data}.  This replaced with no title, \code{main = " "}, or text of the user's choice, \code{main = "text string".} }
  \item{main}{ by default a plot title of \sQuote{Multifractality Plot} is provided.  This replaced with no title, \code{main = " "}, or text of the user's choice, \code{main = "text string".} }
  \item{ifrev}{ by default the empirical C-N function is plotted from highest value to lowest, \code{ifrev = FALSE}.  As the C-N plot is a log-log display this provides greater detail for the highest values.  The direction of accumulation can be key in detecting multifractal patterns, it is usually informative to also prepare a plot with \code{ifrev = TRUE}, i.e. accumulation from lowest to highest values.  To see a dramatic example of this, run the Examples below. }
  \item{xlim}{ by default this is the range of the data with any NAs removed, \code{xlim = range (xx, ra.rm = TRUE)}.  When called from \code{caplot} or \code{shape.alt} an appropriate value for \code{xlim} is determined by the calling functions to ensure all four panels have the same x-axis scaling. \code{xlim} may be defined, see Note below. }
   \item{\dots}{ further arguments to be passed to methods.  For example, the size of the axis scale annotation can be changed by setting \code{cex.axis}, the size of the axis titles by setting \code{cex.lab}, and the size of the plot title by setting \code{cex.main}.  For example, if it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%.  By default individual data points are marked by a plus sign, \code{pch = 3}, if a cross or open circle is desired, then set \code{pch = 4} or \code{pch = 1}, respectively. See \code{\link{display.marks}} for all available symbols. }
}
\details{
Any multi-fractality in the data, will be revealed as 'lines' of data points with different slopes. }
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are removed prior to displaying the plot. 

If the default selection for \code{xlim} is inappropriate it can be set, e.g., \code{xlim = c(0, 200)} or \code{c(2, 200)}, the latter being appropriate for a logarithmcally scaled plot, i.e. \code{log = TRUE}.  If the defined limits lie within the observed data range truncated plots will be displayed.  If this occurs the number of data points omitted is displayed below the total number of observations in the various panels.  

If it is desired to prepare a display of data falling within a defined part of the actual data range, then either a data subset can be prepared externally using the appropriate R syntax, or \code{xx} may be defined in the function call as, for example, \code{Cu[Cu < some.value]} which would remove the influence of one or more outliers having values greater than \code{some.value}.  In this case the number of data values displayed will be the number that are \code{<some.value}.

In some R installations the generation of multi-panel displays and the use of function eqscplot from package MASS causes warning messages related to graphics parameters to be displayed on the current device. These may be suppressed by entering \code{options(warn = -1)} on the R command line, or that line may be included in a \sQuote{first} function prepared by the user that loads the \sQuote{rgr} package, etc.

For summary statistics displays to complement the graphics see, \code{\link{gx.summary1}}, \code{\link{gx.summary2}} and \code{\link{inset}}.
}
\author{ Robert G. Garrett }
\references{
Agterberg, F.P., 1995. Multifractal Modeling of the Sizes and Grades of Giant and Supergiant Deposits. International Geology Review, Vol. 37, No. 1, pp. 1-8.
}
\seealso{ \code{\link{gx.hist}}, \code{\link{cnpplt}}, \code{\link{gx.ecdf}}, \code{\link{gx.mf}}, \code{\link{remove.na}}, \code{\link{display.lty}}, \code{\link{display.marks}}, \code{\link{ltdl.fix.df}}, \code{\link{inset}} }  
\examples{
## Make test data available
data(kola.o)
attach(kola.o)

## Generates an initial display to have a first look at the data and 
## decide how best to proceed
gx.mf(Cu)

## Provides a more appropriate initial display and indicates the 
## quartiles
gx.mf(Cu, xlab = "Cu (mg/kg) in <2 mm O-horizon soil", ifqs = TRUE)

## Causes the C-N plot to be cumulated in reverse order.  This will reveal
## any multi-fractal properties of the data at lower concentrations
gx.mf(Cu, xlab = "Cu (mg/kg) in <2 mm O-horizon soil", ifrev = TRUE)

## Detach test data
detach(kola.o)
}
\keyword{ hplot }
 

