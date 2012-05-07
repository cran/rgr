\name{gx.mvalloc}
\alias{gx.mvalloc}
\title{ Function for Allocation on the basis of Multivariate Data }
\description{
Function to allocate individuals (observations, cases or samples) into one of up to nine (9) reference groups (populations) on the basis of their Mahalanobis distances.  If an individual's predicted probability of group membership (typicality) falls below a user defined \sQuote{cut-off}, \code{pcrit}, the individual is allocated to an \sQuote{outlier bin}.
}
\usage{
gx.mvalloc(pcrit = 0.05, x, ...)
}
\arguments{
  \item{pcrit}{ the critical cut-off probability for group membership below which an individual will be classified as an \sQuote{outlier}.  By default the critical probability of group membership is set to \code{pcrit = 0.05}. }
  \item{x}{ a n by p matrix containing the n individuals, with p variables determined on each, to be allocated, see Details below. }
  \item{\dots}{ a list of objects, up to a maximum of nine (9), saved from any of functions \code{\link{gx.md.gait}}, \code{\link{gx.md.gait.closed}}, \code{\link{gx.mva}}, \code{\link{gx.robmva}} or \cr\code{\link{gx.robmva.closed}}, containing the vectors of means and covariance matrices for the \sQuote{reference} groups into which the individuals are to be classified. }
}
\details{
It is imperative that the data matrix \code{x} contains no special codes and all records (individuals) with NAs have been removed, see Notes below.  It is also imperative that the variables in the reference groups and in the matrix \code{x} of individuals to be classified are identical and in the same order. 

The allocations are made on the assumption that the covariance structures are inhomogeneous, i.e. that the population hyperellipsoids are of different size, shape and orientation in p-space.
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
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed from the matrix \code{x} prior to executing this function, see \code{\link{ltdl.fix.df}}.  Additionally, any rows in the data matrix with \code{NA}s also must have been removed prior to computations, see \code{\link{na.omit}} and \code{\link{remove.na}}.

It is recommended that when applying this procedure to compositional data an ilr transformation be undertaken, this can be done at execution time, see Example below.  This implies that the reference group means and covariance matrices must have also been estimated following an ilr transformation. 
}
\references{
Garrett, R.G., 1990. A robust multivariate allocation procedure with applications to geochemical data. In Proc. Colloquium on Statistical Applications in the Earth Sciences (Eds F.P. Agterberg & G.F. Bonham-Carter). Geological Survey of Canada Paper 89-9, pp. 309-318.

Reimann, C., Filzmoser, P., Garrett, R. and Dutter, R., 2008. Statistical Data Analysis Explained: Applied Environmental Statistics with R. John Wiley & Sons, Ltd., 362 p.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.md.gait}}, \code{\link{gx.md.gait.closed}}, \code{\link{gx.mva}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}}, \code{\link{gx.mvalloc.print}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{na.omit}} }
\examples{
## Generate three groups of synthetic bivariate normal data
grp1 <- mvrnorm(100, mu = c(40, 30), Sigma = matrix(c(6, 3, 3, 2), 2, 2))
grp1 <- cbind(grp1, rep(1, 100))
grp2 <- mvrnorm(100 ,mu = c(50, 40), Sigma = matrix(c(4, -3, -3, 5), 2, 2))
grp2 <- cbind(grp2, rep(2, 100))
grp3 <- mvrnorm(100 ,mu = c(30, 45), Sigma = matrix(c(6, 4, 4, 5), 2, 2))
grp3 <- cbind(grp3 ,rep(3, 100))
## Generate a set of six (6) outliers
anom <- matrix(c(35, 40, 25, 60, 25, 60, 35, 40, 25, 60, 60, 25),6, 2)
anom <- cbind(anom, rep(4, 6))
## Bind the test data sets together and display the test data 
test.mvalloc.mat <- rbind(grp1, grp2, grp3, anom)
test.mvalloc <- as.data.frame(test.mvalloc.mat)
dimnames(test.mvalloc)[[2]] <- c("x","y","grp")
attach(test.mvalloc)
xyplot.tags(x, y, grp, cex = 0.75)

## Generate robust reference groups 
test.save.grp1 <- gx.md.gait(grp1[, -3], mcdstart = TRUE)
test.save.grp2 <- gx.md.gait(grp2[, -3], mcdstart = TRUE)
test.save.grp3 <- gx.md.gait(grp3[, -3], mcdstart = TRUE)

## Allocate the synthetic data into the three reference groups
test.save.mvalloc <- gx.mvalloc(pcrit = 0.05, test.mvalloc.mat[,-3],
test.save.grp1, test.save.grp2, test.save.grp3)
## Display the results of the allocation
xyplot.tags(x, y, test.save.mvalloc$xalloc, cex = 0.75)
gx.mvalloc.print(test.save.mvalloc)

## Save the allocation as a csv file
gx.mvalloc.print(test.save.mvalloc, ifprint = FALSE,
file = "test.mvalloc")

## Clean-up and detach synthetic test data
rm(grp1)
rm(grp2)
rm(grp3)
rm(anom)
rm(test.mvalloc)
rm(test.save.grp1)
rm(test.save.grp2)
rm(test.save.grp3)
rm(test.save.mvalloc)
detach(test.mvalloc)
} 
\keyword{ multivariate }

