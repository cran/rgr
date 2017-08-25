\name{gx.symm.coords}
\alias{gx.symm.coords}
\title{ Computation of Symmetric Coordinates }
\description{
Computes symmetric coordinates for the first two columns of a \code{n} by \code{p} matrix of compositional data.
}
\usage{
gx.symm.coords(x)
}
\arguments{
  \item{x}{ matrix for which the symmetriccoordinates for the first two columns will be computed. }
}
\value{
  \item{Z.av}{ the \code{n} by \code{2} matrix of symmetric coordinates. }
}
\references{
Kynclova, P., Hron, K., and Filzmoser, P., 2017. Correlation Between Compositional Parts Based on Symmetric Balances. Mathematical Geosciences, 49:777-796.
}
\author{ Based on function \code{pivotCoords} from package \code{robCompositions} ver 2.0.3 by Templ, M., Hron, K. and Filzmoser, P. }
\seealso{ \code{\link{gx.symm.coords.mat}}, \code{\link{gx.symm.coords.r}} }
\examples{
## Make test data available
data(nockolds)

## Compute and display symmetric coordinates for the first two columns 
## of the data
gx.symm.coords(as.matrix(nockolds))

## Clean-up
rm(nockolds)
}
\keyword{ multivariate }
\keyword{ manip }
