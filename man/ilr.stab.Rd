\name{ilr.stab}
\alias{ilr.stab}
\title{ Compute the Robust ilr Stability for Two Parts of a Composition }
\description{
Function computes the Robust ilr Stability for two parts of a composition following the procedure in Filzmoser et al. (2010), see details below.
}
\usage{
ilr.stab(xx1, xx2, ifwarn = T)
}
\arguments{
  \item{xx1}{ a column vector from a matrix or data frame of compositional data, \code{xx1[1], ..., xx1[n]}. }
  \item{xx2}{ another column vector from the matrix or data frame of compositional data,  \code{xx2[1], ..., xx2[n]}. \code{xx1} and \code{xx2} must be of identical length, \code{n}. }
  \item{ifwarn}{ by default \code{ifwarn = TRUE} which generates a reminder/warning that when carrying out analyses of compositional data all data must be in the same measurement units.  The message can be suppressed by setting \code{ifwarn = FALSE}. }
}
\details{
The ilr transform of two parts of a composition is \code{ilr.xy = 1/(sqrt(2)) * log(x1/x2)}.  The Robust ilr Stability (Filzmoser et al., 2010) is computed from the MAD of the \code{ilr.xy} values.  This is normalized into the \code{(0,1)} interval as \code{exp(-ilr.MAD * ilr.MAD)}, following the procedure of Buccianti and Pawlowsky-Glahn (2005).  
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any data vectors (rows) containing \code{NA}s are removed prior to computation.
}
\references{
Buccianti, a. and Pawlowsky-Glahn, V., 2005. New perspectives on water chemistry and compositional data analysis. Mathematical Geology, 37(7), 703-727.

Filzmoser, P, Hron, K. and Reimann, C., 2010. The bivariate statistical analysis of environmental (compositional) data. Science of the Total Environment, 408(19), 4230-4238. 
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}} }
\examples{ 
## Make test data available
data(sind)
attach(sind)

## Compute Robust ilr Stability
ilr.stab(Cu, Zn)

## Detach test data
detach(sind)
}
\keyword{ multivariate }
