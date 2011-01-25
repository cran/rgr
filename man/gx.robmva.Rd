\name{gx.robmva}
\alias{gx.robmva}
\title{ Function to undertake a Robust Exploratory Multivariate Data Analysis }
\description{
The function carries out a robust Principal Components Analysis (PCA) and estimates the Mahalanobis distances for a non-compositional dataset and places them in an object to be saved and post-processed for display and further manipulation.  Robust procedures are used, \sQuote{MCD}, \sQuote{MVE} or user supplied weights, for classical procedures see \code{\link{gx.mva}}.  For results display see \code{\link{gx.rqpca.screeplot}}, \code{\link{gx.rqpca.plot}}, \code{\link{gx.rotate}}, \code{\link{gx.md.plot}} and \code{\link{gx.md.print}}.  For closed compositional data sets, such as geochemical analyses, use \code{\link{gx.robmva.closed}}.
}
\usage{
gx.robmva(xx, proc = "mcd", wts = NULL, main = deparse(substitute(xx)))
}
\arguments{
  \item{xx}{ a n by p data matrix to be processed. }
  \item{proc}{ by default \code{proc = "mcd"} for the Minimimum Covariance Determinant (MCD) robust procedure.  Setting \code{proc = "mve"} results in the Minimum Volume Ellipsoid (MVE) procedure being used.  If \code{p > 50} the MVE procedure is used.  See \code{wts} below. }
  \item{wts}{ by default \code{wts = NULL} and the MCD or MVE estimation procedures will be used.  If, however, a vector of \code{n} \code{0} or \code{1} weights are supplied these will be used for robust estimation and the value of \code{proc} ignored. } 
  \item{main}{ by default the name of the object \code{xx}, \code{main = deparse(substitute(xx))}, it may be replaced by the user, but this is not recommended, see Details below. }
}
\details{
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
  \item{ifilr}{ flag for \code{gx.md.plot}, set to \code{FALSE}. }
  \item{matnames}{ the row numbers and column headings of the input matrix. }
  \item{wts}{ the vector of weights for the n individuals arising from the robust estimation of the covariance matrix and means. }
  \item{mean}{ the vector the weighted means for the p variables. }
  \item{cov}{ the p by p weighted covariance matrix for the n by p data matrix. }
  \item{sd}{ the vector of weighted standard deviations for the p variables. }
  \item{snd}{ the n by p matrix of weighted standard normal deviates. }
  \item{r}{ the p by p matrix of weighted Pearson product moment correlation coefficients. }
  \item{eigenvalues}{ the vector of p \code{eigenvalues} of the scaled Pearson robust correlation matrix for RQ analysis, see Grunsky (2001). }
  \item{econtrib}{ the vector of p robustly estimated \code{eigenvalues} each expressed as a percentage of the sum of the eigenvalues. }
  \item{eigenvectors}{ the n by p matrix of robustly estimated \code{eigenvectors}. }
  \item{rload}{ the p by p matrix of robust Principal Component (PC) loadings. }
  \item{rcr}{ the p by p matrix containing the percentages of the variability of each variable (columns) expressed in each robust PC (rows). } 
  \item{rqscore}{ the n by p matrix of the n individuals scores on the p robust PCs. }
  \item{vcontrib}{ a vector of p variances of the columns of \code{rqscore}. }
  \item{pvcontrib}{ the vector of p variances of the columns of \code{rqscore} expressed as percentages.  This is a check on vector \code{econtrib}, the values should be identical for a classical PCA.  However, for robust PCAs this is not so as the trimmed individuals from the robust estimation have been re-introduced.  As a consequence \code{pvcontrib} can be very different from \code{econtrib}.  The plotting of PCs containing high proportions of the variance in robust PCAs can be useful for identifying outliers }
  \item{cpvcontrib}{ the vector of p cumulative sums of \code{pvcontrib}, see above. }
  \item{md}{ the vector of n robust Mahalanobis distances (MDs) for the n by p input matrix. }
  \item{ppm}{ the vector of n robust predicted probabilities of population membership, see Garrett (1990). }
  \item{epm}{ the vector of n robust empirical Chi-square probabilities for the MDs. }
  \item{nr}{ the number of PCs that have been rotated.  At this stage of a data analysis \code{nr = NULL} in order to control PC plot axis labelling. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data matrix, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any rows in the data matrix with \code{NA}s are removed prior to computions.  In the instance of a compositional data opening transformation \code{NA}s have to be removed prior to undertaking the transformation, see \code{\link{na.omit}}, \code{\link{where.na}} and \code{\link{remove.na}}.  When that procedure is followed the opening transformations may be executed on calling the function, see Examples below.

Passing a set of weights from an investigation with \code{\link{gx.md.gait}} or on the basis of some prior knowledge permits the use of a \code{\link{clr}} transformation.  In this instance a Moore-Penrose inverse is computed and used for the estimation of Mahalanobis distances.  See example below.  With refrerence to weights based on prior knowledge, the weights are not necessarily constrained to be \sQuote{0} or \sQuote{1}, intermediate values may be employed.

Executing a \code{\link{clr}} transformation leads to both collinearity and singularity such that neither a PCA can be undertaken or Mahalanobis distances be estimated.  The function fails - do not use with a \code{\link{clr}} transformation.

Executing a \code{\link{ilr}} transformation permits the estimation of both Principal Components and Mahalanobis distances and associated probabilities through the use of (p-1) synthetic variables.  However, in that instance the loadings of the (p-1) synthetic variables will be plotted by \code{\link{gx.rqpca.plot}} rather than the loadings for the elements.

Warnings are generated when the number of individuals (observations, cases or samples) falls below 5*p, and additional warnings when the number of individuals falls below 3*p.  At these low ratios of individuals to variables the shape of the p-space hyperellipsoid is difficult to reliably define, and therefor the results may lack stability.  These limits 5*p and 3*p are generous, the latter especially so; many statisticians would argue that the number of individuals should not fall below 9*p, see Garrett (1993).
}
\references{
Garrett, R.G., 1990. A robust multivariate allocation procedure with applications to geochemical data. In Proc. Colloquium on Statistical Applications in the Earth Sciences (Eds F.P. Agterberg & G.F. Bonham-Carter). Geological Survey of Canada Paper 89-9, pp. 309-318.

Garrett, R.G., 1993. Another cry from the heart. Explore - Assoc. Exploration Geochemists Newsletter, 81:9-14.

Grunsky, E.C., 2001. A program for computinRQ-mode principal components analysis for S-Plus and R. Computers & Geosciences, 27(2):229-235.

Reimann, C., Filzmoser, P., Garrett, R. and Dutter, R., 2008. Statistical Data Analysis Explained: Applied Environmental Statistics with R. John Wiley & Sons, Ltd., 362 p.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{na.omit}}, \code{\link{gx.rqpca.screeplot}}, \code{\link{gx.rqpca.plot}}, \cr\code{\link{gx.rotate}}, \code{\link{gx.md.plot}}, \code{\link{gx.md.print}}, \code{\link{gx.robmva}} }
\examples{
## Make test data available
data(sind)
attach(sind)
sind.mat <- as.matrix(sind[, -c(1:3)])
## Ensure all data are in the same units (mg/kg)
sind.mat2open <- sind.mat
sind.mat2open[, 2] <- sind.mat2open[, 2] * 10000

## Generate gx.robmva object after an clr transform using prior weights
sind.wts <- rep(1, 25)
## Set weights of all 6 high weighted sums, truly anomalous, individuals
## to zero
sind.wts[c(9, 12, 13, 15, 16, 21)] <- 0
sind.save.rob.clr <- gx.robmva(clr(sind.mat2open), wts = sind.wts)
## Display saved object with alternate main titles
gx.rqpca.screeplot(sind.save.rob.clr,
main = "Howarth & Sinding-Larsen\nStream Sediments, clr Transformed Data",
cex.main = 0.8)
gx.rqpca.plot(sind.save.rob.clr,
main = "Howarth & Sinding-Larsen\nStream Sediments, clr Transformed Data",
cex.main = 0.8)
gx.md.plot(sind.save.rob.clr,
main = "Howarth & Sinding-Larsen\nStream Sediments\nclr Transformed Data",
cex.main = 0.8, cex.lab = 0.9)
gx.md.print(cbind(sind.save.rob.clr$md,sind.save.rob.clr$ppm,
ID, Zn, Cu, Cd, Fe, Mn), pcut = 0.5)

## Generate gx.robmva object after an ilr transform
## ** Function gx.robmva.closed should be used **
## ** This is for demonstration only **
sind.save.rob.ilr <- gx.robmva(ilr(sind.mat2open))
## Display saved object with alternate main titles
gx.rqpca.screeplot(sind.save.rob.ilr,
main = "Howarth & Sinding-Larsen\nStream Sediments, ilr Transformed Data",
cex.main = 0.8)
gx.rqpca.plot(sind.save.rob.ilr,
main = "Howarth & Sinding-Larsen\nStream Sediments, ilr Transformed Data",
cex.main = 0.8)
gx.md.plot(sind.save.rob.ilr,
main = "Howarth & Sinding-Larsen\nStream Sediments\nilr Transformed Data",
cex.main = 0.8, cex.lab = 0.9)
gx.md.print(cbind(sind.save.rob.ilr$md,sind.save.rob.ilr$ppm,
ID, Zn, Cu, Cd, Fe, Mn), pcut = 0.5)

## Clean-up and detach test data
rm(sind.mat)
rm(sind.mat2open)
rm(sind.wts)
rm(sind.save.rob.clr)
rm(sind.save.rob.ilr)
detach(sind)
}
\keyword{ multivariate }
\keyword{ robust }
