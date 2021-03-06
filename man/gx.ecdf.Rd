\name{gx.ecdf}
\alias{gx.ecdf}
\title{ Emprical Cumulative Distribution Function (ECDF) }
\description{
Displays an empirical cumulative distribution function (ECDF) plot with a zero-to-one linear y-scale as part of the multi-panel display provided by \code{\link{shape}}.  The function may also be used stand-alone.
}
\usage{
gx.ecdf(xx, xlab = deparse(substitute(xx)), 
	ylab = "Empirical Cumulative Distribution Function", log = FALSE,
	xlim = NULL, main = "", pch = 3, ifqs = FALSE, cex = 0.8, ...)
}
\arguments{
  \item{xx}{name of the variable to be plotted.}
  \item{xlab}{ by default the character string for \code{xx} is used for the x-axis title.  An alternate title can be displayed with \code{xlab = "text string"}, see Examples. }
  \item{ylab}{ a title for the y-axis, defaults to \code{"Emprical Cumulative Distribution Function"}. }
  \item{log}{ to display the data with logarithmic (x-axis) scaling, set \code{log = TRUE}. }
  \item{xlim}{ when used in the \code{\link{shape}} function, \code{xlim} is determined by \code{\link{gx.hist}} and used to ensure all four panels in \code{\link{shape}} have the same x-axis scaling.  However, when used stand-alone the limits may be user-defined by setting \code{xlim}, see Note below. }
  \item{main}{ when used stand-alone a title may be added optionally above the plot by setting \code{main}, e.g., \code{main = "Kola Project, 1995"}. }
  \item{pch}{ by default the plotting symbol is set to a plus, \code{pch = 3}, alternate plotting symbols may be chosen from those displayed by \code{\link{display.marks}}, see also \bold{Notes} below. }
  \item{ifqs}{ setting \code{ifqs = TRUE} results in horizontal and vertical dotted lines being plotted at the three central quartiles and their values, respectively. }
  \item{cex}{ by default the size of the text for data set size, N, is set to 80\%, i.e. \code{cex = 0.8}, and may be changed if required. }
  \item{\dots}{ further arguments to be passed to methods.  The colour of the plotting symbols may be changed from default blach, e.g., \code{col = 2} for red. The size of the axis scale annotation can be change by setting \code{cex.axis}, the size of the axis titles by setting \code{cex.lab}, and the size of the plot title by setting \code{cex.main}.  For example, if it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%. }  
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codesrepresenting blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are removed prior to displaying the plot.

Although the cumulative normal percentage probability (CPP) plot is often the preferred method for displaying the cumulative data distribution as it provides greater detail for inspection in the tails of the data, the ECDF is particularly useful for studying the central parts of data distributions as it has not been compressed to make room for the scale expansion in the tails of a cumulative normal percentage probability (CPP) plot.

If the default selection for \code{xlim} is inappropriate it can be set, e.g., \code{xlim = c(0, 200)} or \code{c(2, 200)}, the latter being appropriate for a logarithmically scale plot, i.e. \code{log = TRUE}.  If the defined limits lie within the observed data range a truncated plot will be displayed.  If this occurs the number of data points omitted is displayed below the total number of observations.  

The available symbols are:\cr
pch: 0 = square, 1 = circle, 2 = triangle, 3 = plus, 4 = X,\cr
     5 = diamond, 6 = upside-down triangle, 7 = square with X,\cr
     8 = asterisk, 9 = diamond with plus, 10 = circle with plus,\cr
     11 = double triangles, 12 = square with plus,\cr
     13 = circle with X, 14 = square with upside-down triangle.\cr
     Symbols 15 to 18 are solid in the colour specified:\cr
     15 = square, 16 = circle, 17 = triangle, 18 = diamond.\cr

If it is desired to prepare a display of data falling within a defined part of the actual data range, then either a data subset can be prepared externally using the appropriate R syntax, or \code{xx} may be defined in the function call as, for example, \code{Cu[Cu < some.value]} which would remove the influence of one or more outliers having values greater than \code{some.value}.  In this case the number of data values displayed will be the number that are \code{<some.value}.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{display.marks}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}} }  
\examples{ 
## Make test data available
data(kola.o)
attach(kola.o)

## Plot a simple ECDF
gx.ecdf(Cu)

## Plot an ECDF with more appropriate labelling and with the quartiles
## indicated
gx.ecdf(Cu , xlab = "Cu (mg/kg) in <2 mm Kola O-horizon soil", log = TRUE, 
ifqs = TRUE)   

## Detach test data
detach(kola.o) 
}
\keyword{ hplot }


