\name{inset}
\alias{inset}
\title{ An EDA Graphical and Statistical Summary }
\description{
Plots a three panel graphical distributional summary for a data set, comprising a histogram and a cumulative normal percentage probability (CPP) plot, together with a table of selected percentiles of the data and summary statistics between them.  Optionally the EDA graphics may be plotted with logarithmic scaling.
}
\usage{
inset(xx, xlab = deparse(substitute(xx)), log = FALSE, xlim = NULL, 
	nclass = NULL, ifnright = TRUE, table.cex = 0.7, ...)
}
\arguments{
  \item{xx}{ name of the variable to be plotted. }
  \item{xlab}{ a title for the x-axis. It is often desirable to replace the default x-axis title of the input variable name text string with a more informative title, e.g., \code{xlab = "Cu (mg/kg) in <2 mm O-horizon soil"}. }
  \item{log}{ if it is required to display the data with logarithmic (x-axis) scaling, set \code{log = TRUE}. }
  \item{xlim}{default limits of the x-axis are determined in the function.  However when used stand-alone the limits may be user-defined by setting \code{xlim}, see Note below. }
  \item{nclass}{ the default procedure for preparing the histogram is to use the Scott (1979) rule. This usually provides an informative histogram, other optional rules are \code{nclass = "sturges"} or \code{nclass = "fd"}; the later standing for Freedman-Diaconis (1981), a rule that is resistant to the presence of outliers in the data.  See Venables and Ripley (2001) for details. }
  \item{ifnright}{ controls where the sample size is plotted in the histogram display, by default this in the upper right corner of the plot.  If the data distribution is such that the upper left corner would be preferable, set \code{ifnright = FALSE}. }
  \item{table.cex}{ on some display devices the table may be \sQuote{cramped}
and the text lines overlap.  If this is true \code{table.cex} can be decreased,
the default is \code{table.cex = 0.85}, conversely it can be increased if the
table appears \sQuote{skinny}. }
  \item{\dots}{ further arguments to be passed to or from methods.  For example, by default individual data points in the CPP plot are marked by a plus sign, \code{pch = 3}, if a cross or open circle is desired, then set \code{pch = 4} or \code{pch = 1}, respectively.  See \code{\link{display.marks}} for all available symbols. Adding \code{ifqs = TRUE} results in horizontal and vertical dotted lines being plotted at the three central quartiles and their values, respectively, in the CPP plot. }
}
\details{
A histogram is displayed on the left, and a cumulative normal percentage probability plot on the right.  Between the two is a table of simple summary statistics, computed in \code{\link{gx.stats}}, including minimum, maximum and percentile values, robust estimates of standard deviation, and the mean, standard deviation and coefficient of variation.  The plots may be displayed with logarithmic axes, however, the summary statistics are not computed with a logarithmic transform.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data vector, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any NAs in the data vector are removed prior to displaying the plot. 

If the default selection for \code{xlim} is inappropriate it can be set, e.g., \code{xlim = c(0, 200)} or \code{c(2, 200)}.  If the defined limits lie within the observed data range a truncated plot will be displayed.  If this occurs the number of data points omitted is displayed below the total number of observations.

The purpose of this function is to prepare publication quality graphics (.wmf) files that can be included in reports or used as inset statistical summaries for maps.  If a series of these are to be prepared the function \code{\link{inset.exporter}} can be used to advantage as it saves a graphics file as part of its procedure.

In some instances if the graphics window has been resized the last line(s) of the table may not be displayed. Resizing the window to be smaller will display the whole table. If the whole table is not visible it will not be saved properly to the graphics file in \code{\link{inset.exporter}}. Once as a complete graphics file the image may be resized in the receiving document.

For summary statistics displays to complement the graphics see, \code{\link{gx.summary1}} and \cr\code{\link{gx.summary2}}.

In some R installations the generation of multi-panel displays and the use of function eqscplot from package MASS causes warning messages related to graphics parameters to be displayed on the current device. These may be suppressed by entering \code{options(warn = -1)} on the R command line, or that line may be included in a \sQuote{first} function prepared by the user that loads the rgr package, etc.
}
\author{ Robert G. Garrett }
\references{ 
Venables, W.N. and Ripley, B.D., 2001. Modern Applied Statistsis with S-Plus, 3rd Edition, Springer, 501 p. See pp. 119 for a description of histogram bin selection computations.
}
\seealso{ \code{\link{gx.hist}}, \code{\link{cnpplt}}, \code{\link{gx.stats}}, \code{\link{inset.exporter}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}} } 
\examples{
## Make test data available
data(kola.o)
attach(kola.o)

## Generates an initial display
inset(Cu)

## Provides a more appropriate display for pubication
inset(Cu, xlab = "Cu (mg/kg) in <2 mm O-horizon soil", log = TRUE)

## NOTE: The example statistics table may not display correctly

## Detach test data
detach(kola.o)
}
\keyword{ hplot }
 

