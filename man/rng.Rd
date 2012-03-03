\name{rng}
\alias{rng}
\title{ Undertakes a Range Transformation on the Columns of a Matrix }
\description{
Function to undertake a range transformation on a data matrix in order that each column is scaled zero-one between the minimum and maximum values.
}
\usage{
rng(xx)
}
\arguments{
  \item{xx}{ a n by p matrix to be range transformed. }
}
\value{
  \item{x}{ a n by p matrix of range-transformed values. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any rows containing \code{NA}s in the data matrix are removed prior to undertaking the transformation.

A range transform may be appropriate for cluster analysis, including 2-d projection displays, applications to ensure all measured variables have equal weight.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{remove.na}} }
\examples{
## Make test data available
data(sind)
sind.mat <- as.matrix(sind[, -c(1:3)])

## Undertake range transform
temp <- rng(sind.mat)
temp

## Clean-up
rm(sind.mat)
rm(temp)
}
\keyword{ manip }
