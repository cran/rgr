\name{gx.mvalloc.closed}
\alias{gx.mvalloc.closed}
\title{ Function for Allocation on the basis of Multivariate Data for closed Compositional Data }
\description{
Function to allocate individuals (observations, cases or samples) from closed compositional, geochemical, data sets into one of up to nine (9) reference groups (populations) on the basis of their Mahalanobis distances.  If an individual's predicted probability of group membership (typicality) falls below a user defined \sQuote{cut-off}, \code{pcrit}, the individual is allocated to an \sQuote{outlier bin}.
}
\usage{
gx.mvalloc.closed(pcrit = 0.05, xx, ...)
}
\arguments{
  \item{pcrit}{ the critical cut-off probability for group membership below which an individual will be classified as an \sQuote{outlier}.  By default the critical probability of group membership is set to \code{pcrit = 0.05}. }
  \item{xx}{ a \code{n} by \code{p} matrix containing the \code{n} individuals, with \code{p} variables determined on each, to be allocated, see Details below. }
  \item{\dots}{ a list of objects, up to a maximum of nine (9), saved from either function \cr\code{\link{gx.md.gait.closed}} or \code{\link{gx.robmva.closed}}, containing the vectors of means and inverse covariance matrices for the \sQuote{reference} groups into which the individuals are to be classified. }
}
\details{
It is imperative that the data matrix \code{xx} contains no special codes, see Note below.  It is also imperative that the variables in the reference groups and in the matrix \code{x} of individuals to be classified are identical in number and in the same order. 

The allocations are made on the assumption that the covariance structures are inhomogeneous, i.e. that the population hyperellipsoids are of different size, shape and orientation in \code{p}-space.
}
\value{
The following are returned as an object to be saved for display with \code{\link{gx.mvalloc.print}}:
  \item{groups}{ a list of the names of the \code{kk} reference groups. }
  \item{kk}{ the number of reference groups passed to the function. }
  \item{n}{ the number of individuals (observations, cases or samples) allocated. }
  \item{p}{ the number of variables in the reference and allocated data. }
  \item{pcrit}{ the critical cut-off probability for reference group membership. }
  \item{pgm}{ a vector of \code{kk} predicted probabilities of reference group memberships. }
  \item{xalloc}{ the reference group, \code{1:kk}, that the individual was allocated into.  All outliers, i.e. all \code{pgm(1:kk) < crit} are allocated to group zero, \code{0}.   Therefore \code{xalloc} will be in the range of \code{0:kk}. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed from the matrix \code{xx} prior to executing this function, see \code{\link{ltdl.fix.df}}.  Any rows in the input data matrix \code{xx} with \code{NA}s are removed prior to computations.
}
\references{
Garrett, R.G., 1990. A robust multivariate allocation procedure with applications to geochemical data. In Proc. Colloquium on Statistical Applications in the Earth Sciences (Eds F.P. Agterberg & G.F. Bonham-Carter). Geological Survey of Canada Paper 89-9, pp. 309-318.

Reimann, C., Filzmoser, P., Garrett, R. and Dutter, R., 2008. Statistical Data Analysis Explained: Applied Environmental Statistics with R. John Wiley & Sons, Ltd., 362 p.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.md.gait.closed}}, \code{\link{gx.robmva.closed}}, \code{\link{gx.mvalloc.print}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{na.omit}} }
\examples{
## Make test data available
data(ogrady)
attach(ogrady)
ogrady.grdr <- gx.subset(ogrady, Lith == "GRDR")
ogrady.grnt <- gx.subset(ogrady, Lith == "GRNT")
## Ensure all data are in the same units (mg/kg)
ogrady.grdr.2open <- ogrady.grdr[, c(5:14)]
ogrady.grdr.2open[, 1:7] <- ogrady.grdr.2open[, 1:7] * 10000
ogrady.grnt.2open <- ogrady.grnt[, c(5:14)]
ogrady.grnt.2open[, 1:7] <- ogrady.grnt.2open[, 1:7] * 10000
ogrady.2open <- ogrady[, c(5:14)]
ogrady.2open[, 1:7] <- ogrady.2open[, 1:7] * 10000 

## Create reference data sets
ogrady.grdr.save <- gx.md.gait.closed(as.matrix(ogrady.grdr.2open),
mcdstart = TRUE)
ogrady.grnt.save <- gx.md.gait.closed(as.matrix(ogrady.grnt.2open),
mcdstart = TRUE)

## Allocate all O'Grady granitoids
ogrady.mvalloc <- gx.mvalloc.closed(pcrit = 0.02, as.matrix(ogrady.2open),
ogrady.grdr.save, ogrady.grnt.save)

## Display list of outliers
gx.mvalloc.print(ogrady.mvalloc)

## Display allocations
ogrady.mvalloc$xalloc

## Save allocations as a csv file
gx.mvalloc.print(ogrady.mvalloc, ifprint = FALSE, file = " ")

## Clean-up and detach test data
rm(ogrady.grdr)
rm(ogrady.grnt)
rm(ogrady.grdr.2open)
rm(ogrady.grnt.2open)
rm(ogrady.2open)
rm(ogrady.grdr.save)
rm(ogrady.grnt.save)
rm(ogrady.mvalloc)
detach(ogrady)
} 
\keyword{ multivariate }

