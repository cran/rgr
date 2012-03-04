\name{gx.pairs4parts}
\alias{gx.pairs4parts}
\title{ Display a Graphical Matrix for Parts of a Compositional Data Set }
\description{
Displays a graphical matrix of log10 scaled x-y plots in the upper triangle and boxplots of the ilr transforms in the lower triangle for the parts of a compositional matrix. The robust ilr stability (Filzmoser et al., 2010) for each x-y pair is displayed as the boxplot title.
}
\usage{
gx.pairs4parts(xx, cex = 2, ifwarn = TRUE, ...)
}
\arguments{
  \item{xx}{ a matrix, or sub-matrix, of parts from a compositional data set. }
  \item{cex}{ by default the size of the text of the variable names in the diagonal of the graphical matrix.  By default \code{cex = 2}, and may be changed if required. }
  \item{ifwarn}{ by default \code{ifwarn = TRUE} which generates a reminder/warning that when carrying out analyses of compositional data all data must be in the same measurement units.  The message can be suppressed by setting \code{ifwarn = FALSE}. }
  \item{\dots}{ further arguments to be passed to plot or bxplot.  For example, the size of the axis scale annotation can be change by setting \code{cex.axis} and the size of the plot title by setting \code{cex.main}.  For example, if it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any data vectors (rows) containing \code{NA}s are removed prior to computation.
}
\references{
Filzmoser, P, Hron, K. and Reimann, C., 2010. The bivariate statistical analysis of environmental (compositional) data. Science of the Total Environment, 408(19), 4230-4238. 
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{bxplot}}, \code{\link{ilr.stab}} }
\examples{ 
## Make test data available
data(sind.mat2open)

## Display 'pairs' plots for a set, or sub-set, of parts of a
## compositional data matrix
gx.pairs4parts(sind.mat2open)
}
\keyword{ multivariate }

