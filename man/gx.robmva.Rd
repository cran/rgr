\name{gx.robmva}
\alias{gx.robmva}
\title{ Function to undertake a Robust Exploratory Multivariate Data Analysis }
\description{
The function carries out a robust Principal Components Analysis (PCA) and estimates the Mahalanobis distances for a non-compositional dataset and places them in an object to be saved and post-processed for display and further manipulation.  For closed compositional, geochemical, data use function \code{\link{gx.robmva.closed}}.  Robust procedures are used, \sQuote{MCD}, \sQuote{MVE} or user supplied weights, for classical procedures see \code{\link{gx.mva}}.  For results display see \code{\link{gx.rqpca.screeplot}}, \code{\link{gx.rqpca.loadplot}}, \code{\link{gx.rqpca.plot}}, \code{\link{gx.rqpca.print}}, \code{\link{gx.md.plot}} and \code{\link{gx.md.print}}.  For Kaiser varimax rotation see \code{\link{gx.rotate}}.
}
\usage{
gx.robmva(xx, proc = "mcd", wts = NULL, main = deparse(substitute(xx)))
}
\arguments{
  \item{xx}{ a \code{n} by \code{p} data matrix to be processed. }
  \item{proc}{ by default \code{proc = "mcd"} for the Minimimum Covariance Determinant (MCD) robust procedure.  Setting \code{proc = "mve"} results in the Minimum Volume Ellipsoid (MVE) procedure being used.  If \code{p > 50} the MVE procedure is used.  See \code{wts} below. }
  \item{wts}{ by default \code{wts = NULL} and the MCD or MVE estimation procedures will be used.  If, however, a vector of \code{n} zero or \code{1} weights are supplied these will be used for robust estimation and the value of \code{proc} ignored. } 
  \item{main}{ by default the name of the object \code{xx}, \code{main = deparse(substitute(xx))}, it may be replaced by the user, but this is not recommended, see Details below. }
}
\details{
If \code{main} is undefined the name of the matrix object passed to the function is used to identify the object.  This is the recommended procedure as it helps to track the progression of a data analysis.  Alternate plot titles are best defined when the saved object is passed to \code{\link{gx.rqpca.screeplot}}, \code{\link{gx.rqpca.loadplot}}, \code{\link{gx.rqpca.plot}} or \code{\link{gx.md.plot}} for display.  If no plot title is required set \code{main = " "}, or if a user defined plot title is required it may be defined, e.g., \code{main = "Plot Title Text"}.

The variances of the robust Principal Component scores are displayed, in a non-robust PCA these decrease with increasing component rank.  However, in a robust PCA this may not be the case, and lower-order scores with high variances are often worthy of further inspection.
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
  \item{matnames}{ the row numbers or identifiers and column headings of the input matrix. }
  \item{wts}{ the vector of weights for the \code{n} individuals arising from the robust estimation of the covariance matrix and means. }
  \item{mean}{ the length \code{p} vector the weighted means for the variables. }
  \item{cov}{ the \code{p} by \code{p} weighted covariance matrix for the \code{n} by \code{p} data matrix. }
  \item{sd}{ the length \code{p} vector of weighted standard deviations for the variables. }
  \item{snd}{ the \code{n} by \code{p} matrix of weighted standard normal deviates. }
  \item{r}{ the \code{p} by \code{p} matrix of weighted Pearson product moment correlation coefficients. }
  \item{eigenvalues}{ the vector of \code{p} eigenvalues of the scaled Pearson robust correlation matrix for RQ analysis, see Grunsky (2001). }
  \item{econtrib}{ the vector of \code{p} robustly estimated eigenvalues each expressed as a percentage of the sum of the eigenvalues. }
  \item{eigenvectors}{ the \code{n} by \code{p} matrix of robustly estimated \code{eigenvectors}. }
  \item{rload}{ the \code{p} by \code{p} matrix of robust Principal Component (PC) loadings. }
  \item{rcr}{ the \code{p} by \code{p} matrix containing the percentages of the variability of each variable (rows) expressed in each robust PC (columns). } 
  \item{rqscore}{ the\code{n} by \code{p} matrix of the \code{n} individuals scores on the \code{p} robust PCs. }
  \item{vcontrib}{ a vector of \code{p} variances of the columns of \code{rqscore}. }
  \item{pvcontrib}{ the vector of \code{p} variances of the columns of \code{rqscore} expressed as percentages.  This is a check on vector \code{econtrib}, the values should be identical for a classical PCA.  However, for robust PCAs this is not so as the trimmed individuals from the robust estimation have been re-introduced.  As a consequence \code{pvcontrib} can be very different from \code{econtrib}.  The plotting of PCs containing high proportions of the variance in robust PCAs can be useful for identifying outliers }
  \item{cpvcontrib}{ the vector of \code{p} cumulative sums of \code{pvcontrib}, see above. }
  \item{md}{ the vector of \code{n} robust Mahalanobis distances (MDs) for the n by p input matrix. }
  \item{ppm}{ the vector of\code{n} robust predicted probabilities of population membership, see Garrett (1990). }
  \item{epm}{ the vector of \code{n} robust empirical Chi-square probabilities for the MDs. }
  \item{nr}{ the number of PCs that have been rotated.  At this stage of a data analysis \code{nr = NULL} in order to control PC plot axis labelling. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any rows in the data matrix with \code{NA}s are removed prior to computions.  In the instance of a compositional data opening transformation \code{NA}s have to be removed prior to undertaking the transformation, see \code{\link{na.omit}}, \code{\link{where.na}} and \code{\link{remove.na}}.  When that procedure is followed the opening transformations may be executed on calling the function, see Examples below.

Passing a set of weights from an investigation with \code{\link{gx.md.gait}} or on the basis of some prior knowledge permits the use of a \code{\link{clr}} transformation.  In this instance a Moore-Penrose inverse is computed and used for the estimation of Mahalanobis distances.  See example below.  With refrerence to weights based on prior knowledge, the weights are not necessarily constrained to be \sQuote{0} or \sQuote{1}, intermediate values may be employed.

Executing a \code{\link{clr}} transformation leads to both collinearity and singularity such that neither a PCA can be undertaken or Mahalanobis distances be estimated.  The function fails - do not use with a \code{\link{clr}} transformation.

Executing a \code{\link{ilr}} transformation permits the estimation of both Principal Components and Mahalanobis distances and associated probabilities through the use of \code{(p-1)} synthetic variables.  However, in that instance the loadings of the \code{(p-1)} synthetic variables will be plotted by \code{\link{gx.rqpca.plot}} rather than the loadings for the elements.

Warnings are generated when the number of individuals (observations, cases or samples) falls below 5*\code{p}, and additional warnings when the number of individuals falls below 3*\code{p}.  At these low ratios of individuals to variables the shape of the \code{p}-space hyperellipsoid is difficult to reliably define, and therefor the results may lack stability.  These limits 5*\code{p} and 3*\code{p} are generous, the latter especially so; many statisticians would argue that the number of individuals should not fall below 9*\code{p}, see Garrett (1993).
}
\references{
Garrett, R.G., 1990. A robust multivariate allocation procedure with applications to geochemical data. In Proc. Colloquium on Statistical Applications in the Earth Sciences (Eds F.P. Agterberg & G.F. Bonham-Carter). Geological Survey of Canada Paper 89-9, pp. 309-318.

Garrett, R.G., 1993. Another cry from the heart. Explore - Assoc. Exploration Geochemists Newsletter, 81:9-14.

Grunsky, E.C., 2001. A program for computing RQ-mode principal components analysis for S-Plus and R. Computers & Geosciences, 27(2):229-235.

Reimann, C., Filzmoser, P., Garrett, R. and Dutter, R., 2008. Statistical Data Analysis Explained: Applied Environmental Statistics with R. John Wiley & Sons, Ltd., 362 p.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{na.omit}}, \code{\link{gx.rqpca.screeplot}}, \code{\link{gx.rqpca.loadplot}}, \code{\link{gx.rqpca.plot}}, \code{\link{gx.rqpca.print}}, \code{\link{gx.md.plot}}, \code{\link{gx.md.print}}, \code{\link{gx.robmva.closed}}, \code{\link{gx.rotate}} }
\examples{
## Generate a population of synthetic bivariate normal data
grp1 <- mvrnorm(100, mu = c(40, 30), Sigma = matrix(c(6, 3, 3, 2), 2, 2))
grp1 <- cbind(grp1, rep(1, 100))
## Generate a set of six (6) outliers
anom <- matrix(c(43, 34, 50, 37, 47, 30, 27, 29, 35, 33, 32, 25),6, 2)
anom <- cbind(anom, rep(2, 6))
## Bind the test data together and display the test data 
test.robmva.mat <- rbind(grp1, anom)
test.robmva <- as.data.frame(test.robmva.mat)
dimnames(test.robmva)[[2]] <- c("x","y","grp")
attach(test.robmva)
xyplot.tags(x, y, dimnames(test.robmva)[[1]], cex = 0.75)

## Generate gx.robmva saved object 
save.rob <- gx.robmva(as.matrix(test.robmva[, c(1:2)]))
## Display saved object with alternate main titles
gx.rqpca.screeplot(save.rob, main = "Bivariate synthetic data")
gx.rqpca.plot(save.rob, cex.lab = 0.8, rowids = TRUE,
main = "Bivariate synthetic data")
gx.md.plot(save.rob, cex.main = 0.9, cex.lab = 0.8, cex.axis = 0.8,
main = "Bivariate synthetic data")
gx.md.print(save.rob, pcut = 0.05)

## Clean-up and detach test data
rm(grp1)
rm(anom)
rm(test.robmva.mat)
rm(test.robmva)
rm(save.rob)
detach(test.robmva)
}
\keyword{ multivariate }
\keyword{ robust }
