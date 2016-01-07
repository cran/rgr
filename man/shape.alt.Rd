\name{shape.alt}
\alias{shape.alt}
\title{ An Alternate EDA Graphical Summary }
\description{
Plots a simple four panel graphical distributional summary for a data set, comprising a histogram, a cumulative normal percentage probability (CPP) plot, an empirical cumulative distribution function (ECDF), and a log-log concentration-number (C-N) plot for multifractality.  Optionally the EDA graphics may be plotted with logarithmic (base 10) scaling, in which case all four plots have identical x-axis scaling.
}
\usage{
shape.alt(xx, xlab = deparse(substitute(xx)), log = FALSE, xlim = NULL,
        nclass = NULL, ifnright = TRUE, ifrev = FALSE, colr = 8, ...)
}
\arguments{
  \item{xx}{ name of the variable to be plotted. }
  \item{xlab}{ by default the character string for \code{xx} is used for the x-axis plot titles.  An alternate title can be displayed with \code{xlab = "text string"}, see Examples. }
  \item{log}{ to display the data with logarithmic (x-axis) scaling, set \code{log = TRUE}. }
  \item{xlim}{ is determined by \code{\link{gx.hist}} and used to ensure all four panels in this function have the same x-axis scaling. \code{xlim} may be defined, see Note below. }
  \item{nclass}{ the default procedure for preparing the histogram depends on sample size.  Where N <= 500 the Scott (1979) rule is used, and when N > 500 the Freedman-Diaconis (1981) rule; both these rules are resistant to the presence of outliers, and usually provide informative histograms.  Alternately, the user may define the histogram binning by setting \code{nclass}, i.e. \code{nclass = "scott"}, \code{nclass = "fd"} or \code{nclass = "sturges"}; the latter being designed for normal distributions (Scott, 1992).  See Venables and Ripley (2001) for details. }
  \item{ifnright}{ controls where the sample size is plotted in the histogram display, by default this in the upper right corner of the plot.  If the data distribution is such that the upper left corner would be preferable, set \code{ifnright = FALSE}.  If \code{ifnright = NULL} there will be no display of the sample size. }
  \item{ifrev}{ by default the empirical C-N function is plotted from highest value to lowest, \code{ifrev = FALSE}.  As the C-N plot is a log-log display this provides greater detail for the highest values.  The direction of accumulation can be key in detecting multifractal patterns, it is usually informative to also prepare a plot with \code{ifrev = TRUE}, i.e. accumulation from lowest to highest values.  To see a dramatic example of this, run the Examples below. }
  \item{colr}{ by default the histogram and Tukey boxplot, or box-and-whisker plot, are infilled in grey, \code{colr = 8}.  If no infill is required, set \code{colr = 0}. See function \code{\link{display.lty}} for the range of available colours. }
  \item{\dots}{ further arguments to be passed to methods.  For example, the size of the axis scale annotation can be changed by setting \code{cex.axis}, the size of the axis titles by setting \code{cex.lab}, and the size of the plot title by setting \code{cex.main}.  For example, if it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%.  By default individual data points in the ECDF and CPP plots are marked by a plus sign, \code{pch = 3}, if a cross or open circle is desired, then set \code{pch = 4} or \code{pch = 1}, respectively. See \code{\link{display.marks}} for all available symbols. Adding \code{ifqs = TRUE} results in horizontal and vertical dotted lines being plotted at the three central quartiles and their values, respectively, in the ECDF and CPP plots.  By default the histogram is infilled in grey, \code{colr = 8}.  If no infill is required, set \code{colr = 0}.  See \code{\link{display.lty}} for the range of available colours. }
}
\details{
A histogram is displayed upper left, and an ECDF is displayed below it (lower left).  To the right of the histogram a cumulative normal percentage probability (CPP) plot is displayed.  Below it (lower right) a log-log C-N plot is displayed to highlight any multifractality in the data, which will be revealed as 'lines' of data points with different slopes.  When log scaling is selected the x-axis scaling is identical in all four plots.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are removed prior to displaying the plots. 

If the default selection for \code{xlim} is inappropriate it can be set, e.g., \code{xlim = c(0, 200)} or \code{c(2, 200)}, the latter being appropriate for a logarithmcally scaled plot, i.e. \code{log = TRUE}.  If the defined limits lie within the observed data range truncated plots will be displayed.  If this occurs the number of data points omitted is displayed below the total number of observations in the various panels.  

If it is desired to prepare a display of data falling within a defined part of the actual data range, then either a data subset can be prepared externally using the appropriate R syntax, or \code{xx} may be defined in the function call as, for example, \code{Cu[Cu < some.value]} which would remove the influence of one or more outliers having values greater than \code{some.value}.  In this case the number of data values displayed will be the number that are \code{<some.value}.

In some R installations the generation of multi-panel displays and the use of function eqscplot from package MASS causes warning messages related to graphics parameters to be displayed on the current device. These may be suppressed by entering \code{options(warn = -1)} on the R command line, or that line may be included in a \sQuote{first} function prepared by the user that loads the \sQuote{rgr} package, etc.

For summary statistics displays to complement the graphics see, \code{\link{gx.summary1}}, \code{\link{gx.summary2}} and \code{\link{inset}}.
}
\author{ Robert G. Garrett }
\references{
Venables, W.N. and Ripley, B.D., 2001. Modern Applied Statistsis with S-Plus, 3rd Edition, Springer, 501 p. See pp. 119 for a description of histogram bin selection computations.
}
\seealso{ \code{\link{gx.hist}}, \code{\link{cnpplt}}, \code{\link{gx.ecdf}}, \code{\link{gx.mf}}, \code{\link{remove.na}}, \code{\link{display.lty}}, \code{\link{display.marks}}, \code{\link{ltdl.fix.df}}, \code{\link{inset}} }  
\examples{
## Make test data available
data(kola.o)
attach(kola.o)

## Generates an initial display to have a first look at the data and 
## decide how best to proceed
shape.alt(Cu)

## Provides a more appropriate initial display and indicates the 
## quartiles
shape.alt(Cu, xlab = "Cu (mg/kg) in <2 mm O-horizon soil", log = TRUE,
	ifqs = TRUE)

## Causes the C-N plot to be cumulated in reverse order.  This will reveal
## any multifractal properties of the data at lower concentrations
shape.alt(Cu, xlab = "Cu (mg/kg) in <2 mm O-horizon soil", log = TRUE, 
	ifrev = TRUE)

## Detach test data
detach(kola.o)
}
\keyword{ hplot }
 

