\name{gx.plot2parts}
\alias{gx.plot2parts}
\title{ Display Plots for Two Parts from a Compositional Data Set }
\description{
Displays a panel of four plots for a pair of parts from a compositional data set.  The displays consist of a log10 scaled x-y plot, a boxplot of the corresponding values of ilr(x,y) annotated with the robust ilr stability measure, and sequential index and ECDF plots of the ilr values.  The display is based on those used in Filzmoser et al. (2010).
}
\usage{
gx.plot2parts(xx1, xx2, x1lab = deparse(substitute(xx1)), 
	x2lab = deparse(substitute(xx2)), cex = 0.8, ifwarn = TRUE, ...)
}
\arguments{
  \item{xx1}{ a column vector from a matrix or data frame of compositional data, \cr\code{xx1[1], ..., xx1[n]}. }
  \item{xx2}{ another column vector from the matrix or data frame of compositional data,  \code{xx2[1], ..., xx2[n]}. \code{xx1} and \code{xx2} must be of identical length, \code{n}. }
  \item{x1lab}{ the x-axis title, by default the variable name, \code{deparse(substitute(xx1))}. It is often desirable to replace this with a more informative title, e.g., \cr\code{x1lab = "Cu (mg/kg) in stream sediment"}. }
  \item{x2lab}{ the y-axis title, by default the variable name, \code{deparse(substitute(xx2))}. It is often desirable to replace this with a more informative title, e.g., \cr\code{x2lab = "Zn (mg/kg) in stream sediment"}. }
  \item{cex}{ by default the size of the text for data set size, N, and the robust ilr stability is set to 80\%, i.e. \code{cex = 0.8}, and may be changed if required. }
  \item{ifwarn}{ by default \code{ifwarn = TRUE} which generates a reminder/warning that when carrying out analyses of compositional data all data must be in the same measurement units.  The message can be suppressed by setting \code{ifwarn = FALSE}. }
  \item{\dots}{ further arguments to be passed to methods.  For example, the size of the axis scale annotation can be change by setting \code{cex.axis}, the size of the axis titles by setting \code{cex.lab}, and the size of the plot title by setting \code{cex.main}.  For example, if it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any data vectors (rows) containing \code{NA}s are removed prior to computation.
}
\references{
Filzmoser, P, Hron, K. and Reimann, C., 2010. The bivariate statistical analysis of environmental (compositional) data. Science of the Total Environment, 408(19):4230-4238. 
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{bxplot}}, \code{\link{gx.ecdf}} }
\examples{ 
## Make test data available
data(sind)
attach(sind)

## Display
gx.plot2parts(Cu, Zn)

## Display with alternate xy-plot titling
gx.plot2parts(Cu, Zn, x1lab = "Cu (mg/kg) in stream sediment",
	x2lab = "Zn (mg/kg) in stream sediment")

## Detach test data
detach(sind)
}
\keyword{ multivariate }
