\name{shape}
\alias{shape}
\title{ An EDA Graphical Summary }
\description{
Plots a simple four panel graphical distributional summary for a data set, comprising a histogram, a horizontal Tukey boxplot or box-and-whisker plot, an empirical cumulative distribution function (ECDF), and a cumulative normal percentage probability (CPP) plot.  The plots in all four panels will have identical x-axis scaling.  Optionally the EDA graphics may be plotted with logarithmic scaling.
}
\usage{
shape(xx, xlab = deparse(substitute(xx)), log = FALSE, 
	xlim = NULL, nclass = "Scott", ifbw = FALSE, wend = 0.05, 
	ifnright = TRUE, colr = 8, cex = 0.8, ...)
}
\arguments{
  \item{xx}{ name of the variable to be plotted. }
  \item{xlab}{ a title for the x-axis. It is often desirable to replace the default x-axis title of the input variable name text string with a more informative title, e.g., \code{xlab = "Cu (mg/kg) in <2 mm O-horizon soil"}. }
  \item{log}{ if it is required to display the data with logarithmic (x-axis) scaling, set \code{log = TRUE}. }
  \item{xlim}{ is determined by \code{\link{gx.hist}} and used to ensure all four panels in this function have the same x-axis scaling. \code{xlim} may be defined, see Note below. }
  \item{nclass}{ the default procedure for preparing the histogram is to use the Scott (1979) rule. This usually provides an informative histogram, other optional rules are \code{nclass = "sturges"} or \code{nclass = "fd"}; the later standing for Freedman-Diaconis (1981), a rule that is resistant to the presence of outliers in the data.  See Venables and Ripley (2001) for details. }
  \item{ifbw}{ the default is to plot a horizontal Tukey boxplot, if a box-and-whisker plot is required set \code{ifbw = TRUE}. }
  \item{wend}{ if \code{ifbw = TRUE} the locations of the whisker-ends have to be defined.  By default these are at the 5th and 95th percentiles of the data, setting \code{wend = 0.02} plots the whisker ends at the 2nd and 98th percentiles. }
  \item{colr}{ by default the histogram is infilled in grey, \code{colr = 8}.  If no infill is required, set \code{colr = 0}. See function \code{\link{display.lty}} for the range of available colours. }
  \item{ifnright}{ controls where the sample size is plotted in the histogram display, by default this in the upper right corner of the plot.  If the data distribution is such that the upper left corner would be preferable, set \code{ifnright = FALSE}. }
  \item{cex}{ by default the size of the text sample size, N, is set to 80\%, i.e. \code{cex = 0.8}, and may be changed if required. }
  \item{\dots}{ further arguments to be passed to methods.  For example, the size of the axis scale annotation can be changed by setting \code{cex.axis}, the size of the axis titles by setting \code{cex.lab}, and the size of the plot title by setting \code{cex.main}.  For example, if it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%.  By default individual data points in the ECDF and CPP plots are marked by a plus sign, \code{pch = 3}, if a cross or open circle is desired, then set \code{pch = 4} or \code{pch = 1}, respectively. See \code{\link{display.marks}} for all available symbols. Adding \code{ifqs = TRUE} results in horizontal and vertical dotted lines being plotted at the three central quartiles and their values, respectively, in the ECDF and CPP plots.  By default the histogram and \sQuote{box} are infilled in grey, \code{colr = 8}.  If no infill is required, set \code{colr = 0}.  See \code{\link{display.lty}} for the range of available colours. }
}
\details{
A histogram is displayed upper left, an ECDF is displayed below it (lower left).  To the right of the histogram a horizontal Tukey boxplot (default) or box-and-whisker plot (option) is displayed (upper right).  In the lower right quadrant a cumulative normal percentage probability (CPP) plot is displayed.  The x-axis scaling is identical in all four plots.

In a box-and-whisker plot there are two special cases.  When \code{wend = 0} the whiskers extend to the observed minima and maxima that are not plotted with the plus symbol.  When \code{wend = 0.25} no whiskers or the data minimum and maximum are plotted, only the median and box representing the span of the middle 50 percent of the data are displayed.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data vector, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are removed prior to displaying the plots. 

If the default selection for \code{xlim} is inappropriate it can be set, e.g., \code{xlim = c(0, 200)} or \code{c(2, 200)}.  If the defined limits lie within the observed data range truncated plots will be displayed.  If this occurs the number of data points omitted is displayed below the total number of observations in the various panels.  

If it is desired to prepare a display of data falling within a defined part of the actual data range, then either a data subset can be prepared externally using the appropriate R syntax, or \code{xx} may be defined in the function call as, for example, \code{Cu[Cu < some.value]} which would remove the influence of one or more outliers having values greater than \code{some.value}.  In this case the number of data values displayed will be the number that are \code{<some.value}.

In some R installations the generation of multi-panel displays and the use of function eqscplot from package MASS causes warning messages related to graphics parameters to be displayed on the current device. These may be suppressed by entering \code{options(warn = -1)} on the R command line, or that line may be included in a \sQuote{first} function prepared by the user that loads the rgr package, etc.

For summary statistics displays to complement the graphics see, \code{\link{gx.summary1}}, \code{\link{gx.summary2}} and \code{\link{inset}}.
}
\author{ Robert G. Garrett }
\references{ 
Venables, W.N. and Ripley, B.D., 2001. Modern Applied Statistsis with S-Plus, 3rd Edition, Springer, 501 p. See pp. 119 for a description of histogram bin selection computations.

Garrett, R.G., 1988. IDEAS - An Interactive Computer Graphics Tool to Assist the Exploration Geochemist. In Current Research Part F, Geological Survey of Canada Paper 88-1F, pp. 1-13 for a description of box-and-whisker plots.
}
\seealso{ \code{\link{gx.hist}}, \code{\link{bxplot}}, \code{\link{gx.ecdf}}, \code{\link{cnpplt}}, \code{\link{remove.na}}, \code{\link{display.lty}}, \code{\link{display.marks}}, \code{\link{ltdl.fix.df}}, \code{\link{inset}} }  
\examples{
## Make test data available
data(kola.o)
attach(kola.o)

## Generates an initial display to have a first look at the data and 
## decide how best to proceed
shape(Cu)

## Provides a more appropriate initial display and indicates the 
## quartiles
shape(Cu, xlab = "Cu (mg/kg) in <2 mm O-horizon soil", log = TRUE,
	ifqs = TRUE)

## Causes the Friedman-Diaconis rule to be used to select the number of
## histogram bins and changes the ECDF and CPP plotting symbols to a
## cross/x
shape(Cu, xlab = "Cu (mg/kg) in <2 mm O-horizon soil", log = TRUE, 
	nclass = "fd", pch = 4)

## Replaces the Tukey boxplot with a box-and-whisker plot where the 
## whiskers extend to the 10th and 90th percentiles and the minimum
## and maximum observed values are marked with a plus sign.
shape(Cu, xlab = "Cu (mg/kg) in <2 mm O-horizon soil", log = TRUE, 
	ifbw =TRUE, wend = 0.1)

## Detach test data
detach(kola.o)
}
\keyword{ hplot }
 

