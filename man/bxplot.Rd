\name{bxplot}
\alias{bxplot}
\title{ Plot a Horizontal Boxplot or Box-and-Whisker Plot }
\description{
Plots a single horizontal boxplot as part of the multi-panel display provided by function \code{\link{shape}}, the default is a Tukey boxplot, alternately a box-and-whisker plot may be displayed.
}
\usage{
bxplot(xx, xlab = deparse(substitute(xx)), log = FALSE,
	ifbw = FALSE, wend = 0.05, xlim = NULL, main = "", colr = 8,
	cex = 1, ...)
}
\arguments{
  \item{xx}{ name of the variable to be plotted. }
  \item{xlab}{ a title for the x-axis. It is often desirable to replace the default x-axis title of the input variable name text string with a more informative title, e.g., \code{xlab = "Cu (mg/kg) in <2 mm O-horizon soil"}. }
  \item{log}{ if it is required to display the data with logarithmic (x-axis) scaling, set \code{log = TRUE}. } 
  \item{ifbw}{ the default is to plot a horizontal Tukey boxplot, if a box-and-whisker plot is required set \code{ifbw = TRUE}. }
  \item{wend}{ if \code{ifbw = TRUE} the locations of the whisker-ends have to be defined.  By default these are at the 5th and 95th percentiles of the data.  Setting \code{wend = 0.02} plots the whisker ends at the 2nd and 98th percentiles. }
  \item{xlim}{ when used in the \code{\link{shape}} function, \code{xlim} is determined by \code{\link{gx.hist}} and used to ensure all four panels in \code{\link{shape}} have the same x-axis scaling.  However when used stand-alone the limits may be user-defined by setting \code{xlim}, see Note below. }
  \item{main}{ when used stand-alone a title may be added optionally above the plot by setting \code{main}, e.g., \code{main = "Kola Project, 1995"}. }
  \item{colr}{ by default the box is infilled in grey, \code{colr = 8}.  If no infill is required, set \code{colr = 0}.  See \code{\link{display.lty}} for the range of available colours. }
  \item{cex}{ by default the size of the text for data set size, N, is set to 80\%, i.e. \code{cex = 0.8}, and may be changed if required. }
  \item{\dots}{ further arguments to be passed to methods.  For example, the size of the axis scale annotation can be change by setting \code{cex.axis}, the size of the axis titles by setting \code{cex.lab}, and the size of the plot title by setting \code{cex.main}.  For example, if it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%. } 
}
\details{
The function can be used stand-alone, but as Tukey boxplots and box-and-whisker plots are usually used to compare the distributions of data subsets the functions \code{\link{tbplots}} (Tukey boxplots) and \code{\link{bwplots}} (box-and-whisker plots) are required for that purpose.

When the boxplot is displayed on a logarithmically scaled x-axis, the data are log transformed prior to the computation of the positions of the fences used in the Tukey boxplot to identify near and far outliers, plotted as plusses and circles, respectively.

In a box-and-whisker plot there are two special cases.  When \code{wend = 0} the whiskers extend to the observed minima and maxima that are not plotted with the plus symbol.  When \code{wend = 0.25} no whiskers or the data minimum and maximum are plotted, only the median and box representing the span of the middle 50 percent of the data are displayed.
}
\note{
Any less than detection limit values represented by negative values, or zeros or numeric codes representing blanks in the data vector, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are removed prior to displaying the plot.

If the default selection for \code{xlim} is inappropriate it can be set, e.g., \code{xlim = c(0, 200)} or \code{c(2, 200)}.  If the defined limits lie within the observed data range a truncated plot will be displayed.  If this occurs the number of data points omitted is displayed below the total number of observations.  

If it is desired to prepare a display of data falling within a defined part of the actual data range, then either a data subset can be prepared externally using the appropriate R syntax, or \code{xx} may be defined in the function call as, for example, \code{Cu[Cu < some.value]} which would remove the influence of one or more outliers having values greater than \code{some.value}.  In this case the number of data values displayed will be the number that are \code{<some.value}. 
}
\author{ Robert G. Garrett }
\references{ 
Garrett, R.G., 1988. IDEAS - An Interactive Computer Graphics Tool to Assist the Exploration Geochemist. In Current Research Part F, Geological Survey of Canada Paper 88-1F, pp. 1-13 for a description of box-and-whisker plots. 
}
\seealso{ \code{\link{shape}}, \code{\link{display.lty}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}} } 
\examples{
## Make test data available
data(kola.o)
attach(kola.o)

## Display a simple boxplot
bxplot(Cu)

## Display a more appropriately labelled and scaled boxplot
bxplot(Cu, xlab = "Cu (mg/kg) in <2 mm Kola O-horizon soil", log = TRUE)

## Display a box-and-whisker plot with whiskers ending at the 2nd and
## 98th percentiles
bxplot(Cu, xlab = "Cu (mg/kg) in <2 mm Kola O-horizon soil", ifbw = TRUE, 
	wend = 0.02, log = TRUE)

## Detach test data
detach(kola.o)
}
\keyword{ hplot }


