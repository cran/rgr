\name{gx.robmva.closed}
\alias{gx.robmva.closed}
\title{ Function to undertake a Robust Closed Data Multivariate EDA }
\description{
The function carries out a robust Principal Components Analysis (PCA) and estimates the Mahalanobis distances for a closed compositional, geochemical, dataset and places the results in an object to be saved and post-processed for display and further manipulation.  Robust procedures are used, \sQuote{MCD}, \sQuote{MVE} or user supplied weights, for classical procedures see \code{\link{gx.mva}}, or for non-compositional data and robust procedures see \code{\link{gx.robmva}}.  For results display see \code{\link{gx.rqpca.screeplot}}, \code{\link{gx.rqpca.plot}}, \code{\link{gx.rqpca.loadplot}}, \code{\link{gx.rotate}}, \cr\code{\link{gx.md.plot}}, and \code{\link{gx.md.print}}.
}
\usage{
gx.robmva.closed(xx, proc = "mcd", wts = NULL,
	main = deparse(substitute(xx)))
}
\arguments{
  \item{xx}{ a n by p data matrix to be processed. }
  \item{proc}{ by default \code{proc = "mcd"} for the Minimimum Covariance Determinant (MCD) robust procedure.  Setting \code{proc = "mve"} results in the Minimum Volume Ellipsoid (MVE) procedure being used.  If \code{p > 50} the MVE procedure is used.  See \code{wts} below. }
  \item{wts}{ by default \code{wts = NULL} and the MCD or MVE estimation procedures will be used.  If, however, a vector of \code{n} \code{0} or \code{1} weights are supplied these will be used for robust estimation and the value of \code{proc} ignored. } 
  \item{main}{ by default the name of the object \code{xx}, \code{main = deparse(substitute(xx))}, it may be replaced by the user, but this is not recommended, see Details below. }
}
\details{
The data are initially isometrically log-ratio transformed and a robust covariance matrix and vector of means estimated, by either the Minimum Covariance Determinant (MCD) or Minimum Volume Elloipsoid (MVE) procedures, or on the basis of a vector of user supplied weights. The Mahalanobis distances are computed on the basis of the ilr transformed data. The ilr transformed data and robust estimates, including the inverse of the covariance matrix, are then back-transformed to the centred log-ratio basis and a Principal Components Analysis (PCA) undertaken (see Filzmoser, et al., 2009), permitting the results to be interpreted in the original variable space.
 
If \code{main} is undefined the name of the matrix object passed to the function is used to identify the object.  This is the recommended procedure as it helps to track the progression of a data analysis.  Alternate plot titles are best defined when the saved object is passed to \code{\link{gx.rqpca.plot}}, \code{\link{gx.rqpca.screeplot}} or \code{\link{gx.md.plot}} for display.  If no plot title is required set \code{main = " "}, or if a user defined plot title is required it may be defined, e.g., \code{main = "Plot Title Text"}.
}
\value{
The following are returned as an object to be saved for subsequent display, etc.:
  \item{main}{ by default (recommended) the input data matrix name. }
  \item{input}{ the data matrix name, \code{input = deparse(substitute(xx))}, retained to be used by post-processing display functions. }
  \item{proc}{ the robust procedure used, the value of \code{proc} will be \code{"mcd"}, \code{"mve"} or \code{"wts"}. }
  \item{n}{ the total number of individuals (observations, cases or samples) in the input data matrix. }
  \item{nc}{ the number of individuals remaining in the \sQuote{core} data subset following the robust estimation, i.e. the sum of those individuals with \code{wts = 1}. }
  \item{p}{ the number of variables on which the multivariate operations were based. }
  \item{ifilr}{ flag for \code{gx.md.plot}, set to \code{TRUE}. }
  \item{matnames}{ the row numbers and column headings of the input matrix. }
  \item{wts}{ the vector of weights for the n individuals arising from the robust estimation of the covariance matrix and means. }
  \item{mean}{ the length p vector of clr-based weighted means for the variables. }
  \item{cov}{ the p by p weighted clr-based covariance matrix for the n by p data matrix. }
  \item{cov.inv}{ the p by p weighted clr-based inverse of the covariance matrix, for use by function gx.mvalloc.closed. }
  \item{sd}{ the length p vector of weighted clr-based standard deviations for the variables. }
  \item{snd}{ the n by p matrix of clr-based weighted standard normal deviates. }
  \item{r}{ the p by p matrix of weighted clr-based Pearson product moment correlation coefficients. }
  \item{eigenvalues}{ the vector of p \code{eigenvalues} of the scaled clr-based Pearson robust correlation matrix for RQ analysis, see Grunsky (2001). }
  \item{econtrib}{ the vector of p robustly estimated \code{eigenvalues} each expressed as a percentage of the sum of the eigenvalues. }
  \item{eigenvectors}{ the n by p matrix ofclr-based robustly estimated \code{eigenvectors}. }
  \item{rload}{ the p by p matrix of robust clr-based Principal Component (PC) loadings. }
  \item{rcr}{ the p by p matrix containing the percentages of the variability of each variable (columns) expressed in each robust clr-based PC (rows). } 
  \item{rqscore}{ the n by p matrix of the n individuals scores on the p robust clr-based PCs. }
  \item{vcontrib}{ a vector of p variances of the columns of \code{rqscore}. }
  \item{pvcontrib}{ the vector of p variances of the columns of \code{rqscore} expressed as percentages.  This is a check on vector \code{econtrib}, the values should be identical for a classical PCA.  However, for robust PCAs this is not so as the trimmed individuals from the robust estimation have been re-introduced.  As a consequence \code{pvcontrib} can be very different from \code{econtrib}.  The plotting of PCs containing high proportions of the variance in robust PCAs can be useful for identifying outliers. }
  \item{cpvcontrib}{ the vector of p cumulative sums of \code{pvcontrib}, see above. }
  \item{md}{ the vector of n robust ilr-based Mahalanobis distances (MDs) for the n by p input matrix. }
  \item{ppm}{ the vector of n robust ilr-based predicted probabilities of population membership, see Garrett (1990). }
  \item{epm}{ the vector of n robust ilr-based empirical Chi-square probabilities for the MDs. }
  \item{nr}{ the number of PCs that have been rotated.  At this stage of a data analysis \code{nr = NULL} in order to control PC plot axis labelling. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any rows in the data matrix with \code{NA}s are removed prior to computations.  In the instance of a compositional data opening transformation \code{NA}s have to be removed prior to undertaking the transformation, see \code{\link{na.omit}}, \code{\link{where.na}} and \code{\link{remove.na}}.  When that procedure is followed the opening transformations may be executed on calling the function, see Examples below.

Warnings are generated when the number of individuals (observations, cases or samples) falls below 5*p, and additional warnings when the number of individuals falls below 3*p.  At these low ratios of individuals to variables the shape of the p-space hyperellipsoid is difficult to reliably define, and therefore the results may lack stability.  These limits 5*p and 3*p are generous, the latter especially so; many statisticians would argue that the number of individuals should not fall below 9*p, see Garrett (1993).
}
\references{
Filzmoser, P., Hron, K., Reimann, C. and Garrett, R., 2009. Robust factor analysis for compositional data. Computers & Geosciences, 35(9):1854-1861.

Garrett, R.G., 1990. A robust multivariate allocation procedure with applications to geochemical data. In Proc. Colloquium on Statistical Applications in the Earth Sciences (Eds F.P. Agterberg & G.F. Bonham-Carter). Geological Survey of Canada Paper 89-9, pp. 309-318.

Garrett, R.G., 1993. Another cry from the heart. Explore - Assoc. Exploration Geochemists Newsletter, 81:9-14.

Grunsky, E.C., 2001. A program for computing RQ-mode principal components analysis for S-Plus and R. Computers & Geosciences, 27(2):229-235.

Reimann, C., Filzmoser, P., Garrett, R. and Dutter, R., 2008. Statistical Data Analysis Explained: Applied Environmental Statistics with R. John Wiley & Sons, Ltd., 362 p.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{na.omit}}, \code{\link{orthonorm}}, \code{\link{gx.rqpca.screeplot}}, \cr\code{\link{gx.rqpca.plot}}, \code{\link{gx.rqpca.loadplot}}, \code{\link{gx.rotate}}, \code{\link{gx.md.plot}}, \code{\link{gx.md.print}}, \cr\code{\link{gx.robmva}} }
\examples{
## Make test data available
data(sind.mat2open)

## Generate gx.robmva.closed object
sind.save <- gx.robmva.closed(sind.mat2open)

## Display Mahalanobis distances
gx.md.plot(sind.save)

## Display default PCA results
gx.rqpca.screeplot(sind.save)
gx.rqpca.plot(sind.save)

## Display appropriately annotated results
gx.md.plot(sind.save,
main = "Howarth & Sinding-Larsen\nStream Sediments, Opened Data",
cex.main=0.8)
gx.rqpca.screeplot(sind.save,
main = "Howarth & Sinding-Larsen Stream Sediments\nOpened Data")
gx.rqpca.plot(sind.save,
main = "Howarth & Sinding-Larsen Stream Sediments\nOpened Data")
gx.rqpca.plot(sind.save, rowids = TRUE, cex = 0.8,
main = "Howarth & Sinding-Larsen Stream Sediments\nOpened Data")
sind.save$pvcontrib
gx.rqpca.plot(sind.save, v1 = 3, v2 =4, rowids = TRUE, cex = 0.8,
main = "Howarth & Sinding-Larsen Stream Sediments\nOpened Data")

## Display Kaiser Varimax rotated (nrot = 4) results
sind.save.rot4 <- gx.rotate(sind.save, 4)
gx.rqpca.plot(sind.save.rot4,
main = "Howarth & Sinding-Larsen Stream Sediments\nOpened Data")
gx.rqpca.plot(sind.save.rot4, rowids = TRUE, cex = 0.8,
main = "Howarth & Sinding-Larsen Stream Sediments\nOpened Data")
gx.rqpca.plot(sind.save.rot4, v1 = 3, v2 =4, rowids = TRUE, cex = 0.8,
main = "Howarth & Sinding-Larsen Stream Sediments\nOpened Data")

## Clean-up
rm(sind.save)
rm(sind.save.rot4)
}
\keyword{ multivariate }
\keyword{ robust }
