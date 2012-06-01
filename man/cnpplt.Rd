\name{cnpplt}
\alias{cnpplt}
\title{ Cumulative Normal Percentage Probability (CPP) Plot }
\description{
Displays a cumulative normal percentage probability (CPP) plot, equivalent to a Q-Q plot, as has been traditionally used by physical scientists and engineers.
}
\usage{
cnpplt(xx, xlab = deparse(substitute(xx)), 
	ylab = "\% Cumulative Probability", log = FALSE, xlim = NULL, 
	main = "", ifqs = FALSE, ifshape = FALSE, pch = 3,
	cex = 0.8, cexp = 1, cex.axis = 0.8, ...)
}
\arguments{
  \item{xx}{ name of the variable to be plotted. }
  \item{xlab}{  a title for the x-axis. It is often desirable to replace the default x-axis title of the input variable text string with a more informative title, e.g., \cr\code{xlab = "Cu (mg/kg) in <2 mm O-horizon soil"}. }
  \item{ylab}{ a title for the y-axis, defaults to \code{"\% Cumulative Probability".} }
  \item{log}{ if it is required to display the data with logarithmic (x-axis) scaling, set \code{log  = TRUE}. }
  \item{xlim}{ when used in the \code{\link{shape}} function, \code{xlim} is determined by function \code{\link{gx.hist}} and used to ensure all four panels in \code{\link{shape}} have the same x-axis scaling.  However, when used stand-alone the limits may be user-defined by setting \code{xlim}, see Details below. }
  \item{main}{ when used stand-alone a title may be added optionally above the plot by setting \code{main}, e.g., \code{main = "Kola Ecogeochemistry Project, 1995"}. }
  \item{ifqs}{ setting \code{ifqs = TRUE} results in horizontal and vertical dotted lines being plotted at the three central quartiles and their values, respectively. }
  \item{ifshape}{ when used with function \code{\link{shape}} or \code{\link{caplot}} to plot into a panel set \code{ifshape = TRUE} to ensure only essential probability scale axis labels are displayed to avoid overplotting on the reduced size panel plot. }
  \item{pch}{ by default the plotting symbol is set to a plus, \code{pch = 3}, alternate plotting symbols may be chosen from those displayed by \code{\link{display.marks}}. }
  \item{cex}{ by default the size of the text for data set size, N, is set to 80\%, i.e. \code{cex = 0.8}, and may be changed if required. }
  \item{cexp}{ by default the size of the plotting symbol, \code{pch}, is set to 100\%, and may be changed if required. }
  \item{cex.axis}{ if overplotting occurs in the y-axis labelling the size of the y-axis labels may be reduced by setting \code{cex.axis} to a number smaller than the default of \code{cex.axis = 0.8}. }
  \item{\dots}{ further arguments to be passed to methods.  For example, the size of the axis titles by setting \code{cex.lab}, and the size of the plot title by setting \code{cex.main}.  For example, if it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%. }  
}
\details{
If the default selection for \code{xlim} is inappropriate it can be set, e.g., \code{xlim = c(0, 200)} or \code{c(2, 200)}, the latter being appropriate for a logarithmically scaled plot, i.e. \code{log  = TRUE}.  If the defined limits lie within the observed data range a truncated plot will be displayed.  If this occurs the number of data points omitted is displayed below the total number of observations. 

If it is desired to prepare a display of data falling within a defined part of the actual data range, then either a data subset can be prepared externally using the appropriate R syntax, or \code{xx} may be defined in the function call as, for example, \code{Cu[Cu < some.value]} which would remove the influence of one or more outliers having values greater than \code{some.value}.  In this case the number of data values displayed will be the number that are \code{<some.value}.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are removed prior to displaying the plot.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{display.marks}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}} }  
\examples{
## Make test data available
data(kola.o)
attach(kola.o)

## A stand-alone cumulative normal percentage probability plot
cnpplt(Cu)

## A more appropriately labelled and scaled cumulative normal percentage
## probability plot using a cross/x rather than a plus
cnpplt(Cu, xlab = "Cu (mg/kg) in <2 mm O-horizon soil", log = TRUE, 
	pch = 4)

## Detach test data
detach(kola.o)
}
\keyword{ hplot }

