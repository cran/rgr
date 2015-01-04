\name{gx.sm}
\alias{gx.sm}
\title{ Display Robust ilr Stabilities and log-ratio Medians for Compositional Data }
\description{
The function computes and displays a matrix of Robust ilr Stabilities (Filzmoser et al., 2010) and medians of log-ratios in the upper and lower triangles, respectively.
}
\usage{
gx.sm(xx, ifwarn = TRUE)
}
\arguments{
  \item{xx}{ a matrix, or sub-matrix, of parts from a compositional data set. }
  \item{ifwarn}{ by default \code{ifwarn = TRUE} which generates a reminder/warning that when carrying out analyses of compositional data all data must be in the same measurement units.  The message can be suppressed by setting \code{ifwarn = FALSE}. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any data vectors (rows) containing \code{NA}s are removed prior to computation.

This function is for used with closed compositional data sets, i.e. geochemical analyses.  For the \sQuote{classical} Aitchison (1984, 1986) approach see \code{\link{gx.vm}}.
}
\references{
Aitchison, J., 1984. The statistical analysis of geochemical compositions. Mathematical Geology, 16(6):531-564.

Aitchison, J., 1986. Ths Statistical Analysis of Compositional data. Chapman and Hall, London, U.K., 416 p.

Filzmoser, P, Hron, K. and Reimann, C., 2010. The bivariate statistical analysis of environmental (compositional) data. Science of the Total Environment, 408(19):4230-4238. 
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{ilr.stab}}, \code{\link{gx.vm}} }
\examples{ 
## Make test data available
data(sind.mat2open)

## Compute Robust ilr Stabilities and log-ratio medians
gx.sm(sind.mat2open)
}
\keyword{ multivariate }

