\name{gx.hist}
\alias{gx.hist}
\title{ Plot a Histogram }
\description{
Plots a histogram for a data set, the user has options for defining the axis and main titles, the x-axis limits, arithmetic or logarithmic x-axis scaling, the method for calculating the number of bins the data are displayed in, and the colour of the infill.
}
\usage{
gx.hist(xx, xlab = deparse(substitute(xx)), 
	ylab = "Number of Observations", log = FALSE, xlim = NULL, 
	main = "", nclass = NULL, colr = NULL, ifnright = TRUE,
	cex = 0.8, ...)
}
\arguments{
  \item{xx}{ name of the variable to be plotted }
  \item{xlab}{ by default the character string for \code{xx} is used for the x-axis title.  An alternate title can be displayed with \code{xlab = "text string"}, see Examples. }
  \item{ylab}{ a default y-axis title of \code{"Number of Observations"} is provided, this may be changed, e.g., \code{ylab = "Counts"}. }
  \item{log}{ to display the data with logarithmic (x-axis) scaling, set \code{log = TRUE}. }
  \item{xlim}{ default limits of the x-axis are determined in the function for use in other panel plots of function \code{shape}.  However, when used stand-alone the limits may be user-defined by setting \code{xlim}, see Note below. }
  \item{main}{ when used stand-alone a title may be added optionally above the plot by setting \code{main}, e.g., \code{main = "Kola Project, 1995"}. }
  \item{nclass}{ the default procedure for preparing the histogram depends on sample size.  Where N <= 500 the Scott (1979) rule is used, and when N > 500 the Freedman-Diaconis (1981) rule; both these rules are resistant to the presence of outliers, and usually provide informative histograms.  Alternately, the user may define the histogram binning by setting \code{nclass}, i.e. \code{nclass = "scott"}, \code{nclass = "fd"} or \code{nclass = "sturges"}; the latter being designed for normal distributions (Scott, 1992).  See Venables and Ripley (2001) for details. }
  \item{colr}{ by default the histogram is infilled in grey, \code{colr = 8}.  If no infill is required, set \code{colr = 0}. See function \code{\link{display.lty}} for the range of available colours. }
  \item{ifnright}{ controls where the sample size is plotted in the histogram display, by default this in the upper right corner of the plot.  If the data distribution is such that the upper left corner would be preferable, set \code{ifnright = FALSE}.  If neither option generates an acceptable plot, setting \code{ifnright = NULL} suppresses the display of the data set size.  }
  \item{cex}{ by default the size of the text for data set size, N, is set to 80\%, i.e. \code{cex = 0.8}, and may be changed if required. }
  \item{\dots}{ further arguments to be passed to methods.  For example, the size of the axis titles may be changed by setting \code{cex.lab}, the size of the axis labels by setting \code{cex.axis}, and the size of the plot title by setting \code{cex.main}.  For example, if it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%. }
}
\value{
  \item{xlim}{ A two element vector containing the actual minimum [1] and maximum [2] x-axis limits used in the histogram display are returned.  These are used in function \code{\link{shape}} to ensure all panels have the same x-axis limits. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.
  
Any \code{NA}s in the data vector are removed prior to displaying the plots. 

If the default selection for \code{xlim} is inappropriate it can be set, e.g., \code{xlim = c(0, 200)} or \code{c(2, 200)}, the latter being appropriate for a logarithmically scaled plot, i.e. \code{log = TRUE}.  If the defined limits lie within the observed data range a truncated plot will be displayed.  If this occurs the number of data points omitted is displayed below the total number of observations.  

If it is desired to prepare a display of data falling within a defined part of the actual data range, then either a data subset can be prepared externally using the appropriate R syntax, or \code{xx} may be defined in the function call as, for example, \code{Cu[Cu < some.value]} which would remove the influence of one or more outliers having values greater than \code{some.value}.  In this case the number of data values displayed will be the number that are \code{<some.value}.
}
\author{ Robert G. Garrett }
\references{
Venables, W.N. and Ripley, B.D., 2001. Modern Applied Statistics with S-Plus, 3rd Edition, Springer, 501 p. See pp. 119 for a description of histogram bin selection computations. 
}
\seealso{ \code{\link{display.lty}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}} }    
\examples{
## Make test data available
data(kola.o) 
attach(kola.o)

## Generates an initial display to have a first look at the data and
## decide how best to proceed
gx.hist(Cu)

## Provides a more appropriate initial display
gx.hist(Cu, xlab = "Cu (mg/kg) in <2 mm Kola O-horizon soil", log = TRUE)

## Causes the Sturges rule to be used to select the number 
## of histogram bins
gx.hist(Cu, xlab = "Cu (mg/kg) in <2 mm Kola O-horizon soil", log = TRUE, 
	nclass = "sturges")

## Detach test data
detach(kola.o)
}
\keyword{ hplot }


 
   
    
     

    

