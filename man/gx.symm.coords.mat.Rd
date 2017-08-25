\name{gx.symm.coords.mat}
\alias{gx.symm.coords.mat}
\title{ Create a Matrix of Symmetric Coordinates }
\description{
Function to compute a matrix of symmetric coordinates for any two parts, \code{v1} and \code{v2}, of a \code{n} by \code{p} data frame or matrix of compositions.
}
\usage{
gx.symm.coords.mat(x, v1, v2)
}
\arguments{
  \item{x}{ a data frame or matrix of compositions. }
  \item{v1}{ the index of the first part for which symmetric coordinates are required. }
  \item{v2}{ the index of the second part for which symmetric coordinates are required. }
}
\value{
  \item{z}{ a \code{n} by \code{2} matrix containing the symmetric coordinates of parts \code{v1} and \code{v2}.  The column names are the names of the parts in the input data frame or matrix \code{x}. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

For compositional data analysis all the data must be in the same measurement units.
}
\author{ Robert G. Garrett }
\references{
Kynclova, P., Hron, K., and Filzmoser, P., 2017. Correlation Between Compositional Parts Based on Symmetric Balances. Mathematical Geosciences, 49:777-796.
}
\seealso{ \code{\link{gx.symm.coords}} }
\examples{
## Make test data available
data(nockolds)

## Compute and display symmetric coordinates for Ca and Na
gx.symm.coords.mat(nockolds, 6, 7)

## Detach test data
rm(nockolds)
}
\keyword{ multivatiate }
\keyword{ manip }
