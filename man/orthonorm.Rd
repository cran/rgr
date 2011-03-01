\name{orthonorm}
\alias{orthonorm}
\title{ Computation of an Orthonormal Basis Matrix }
\description{
Computes an othonormal basis matrix to be used for the back-transformation of ilr-based data and statistics to clr-based data and statistics.
}
\usage{
orthonorm(p)
}
\arguments{
  \item{p}{ the dimension of the p-space, the number of original variables. }
}
\value{
  \item{V}{ the p by (p-1) orthormal basis matrix. }
}
\references{
Filzmoser, P., Hron, K., Reimann, C. and Garrett, R., 2009. Robust factor analysis for compositional data. Computers & Geosciences, 35(9):1854-1861.
}
\author{ Based on a function by Peter Filzmoser and Karel Hron }
\seealso{ \code{\link{ilr}}, \code{\link{clr}}, \code{\link{gx.robmva.closed}}, \code{\link{gx.md.gait.closed}} }
\examples{
## Make test data available
data(sind)
attach(sind)
sind.mat <- as.matrix(sind[, -c(1:3)])
## Ensure all data are in the same units (mg/kg)
sind.mat2open <- sind.mat
sind.mat2open[, 2] <- sind.mat2open[, 2] * 10000

## Compute and display clr transformed data
prmatrix(clr(sind.mat2open))

## Compute ilr transformed data
sind.ilr <-ilr(sind.mat2open)

## Compute orthonormal basis matrix, sind is a 25 by 6 data set
V <- orthonorm(6)
prmatrix(V)

## Back-transform ilr transformed data to clr form and display
temp <- sind.ilr \%*\% t(V)
dimnames(temp)[[2]] <- dimnames(sind.mat)[[2]]
prmatrix(temp)

## Clean-up and detach test data
rm(sind.mat)
rm(sind.mat2open)
rm(sind.ilr)
rm(V)
detach(sind)
}
\keyword{ multivariate }
\keyword{ manip }
