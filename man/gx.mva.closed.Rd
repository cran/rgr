\name{gx.mva.closed}
\alias{gx.mva.closed}
\title{ Function to undertake an Exploratory Multivariate Data Analysis on Compositional, geochemical data }
\description{
The function carries out a Principal Components Analysis (PCA) and estimates the Mahalanobis distances for a compositional dataset and places them in an object to be saved and post-processed for display and further manipulation.  Classical procedures are used, for robust procedures see \code{\link{gx.robmva.closed}}.  For results display see \code{\link{gx.rqpca.screeplot}}, \code{\link{gx.rqpca.loadplot}}, \code{\link{gx.rqpca.plot}}, \code{\link{gx.rqpca.print}}, \code{\link{gx.md.plot}} and \code{\link{gx.md.print}}.  For Kaiser varimax rotation see \code{\link{gx.rotate}}.
}
\usage{
gx.mva.closed(xx, main = deparse(substitute(xx)))
}
\arguments{
  \item{xx}{ a \code{n} by \code{p} data matrix to be processed. }
  \item{main}{ by default the name of the object \code{xx}, \code{main = deparse(substitute(xx))}, it may be replaced by the user, but this is not recommended, see Details below. }
}
\details{
If \code{main} is undefined the name of the matrix object passed to the function is used to identify the object.  This is the recommended procedure as it helps to track the progression of a data analysis.  Alternate plot titles are best defined when the saved object is passed to \code{\link{gx.rqpca.loadplot}}, \code{\link{gx.rqpca.plot}}, \code{\link{gx.rqpca.screeplot}} or \code{\link{gx.md.plot}} for display.  If no plot title is required set \code{main = " "}, or if a user defined plot title is required it may be defined, e.g., \code{main = "Plot Title Text"}.

The data are centre log-ratio transformed prior to undertaking the PCA.  For the computation of Mahalanobis distances the data are isometrically log-ratio transformed, this results in the loss of one degree of freedom.
}
\value{
The following are returned as an object to be saved for subsequent display, etc.:
  \item{main}{ by default (recommended) the input data matrix name. }
  \item{input}{ the data matrix name, \code{input = deparse(substitute(xx))}, retained to be used by post-processing display functions. }
  \item{proc}{ the procedure used, by default \code{proc = "cov"} to indicate a classical covariance matrix. }
  \item{n}{ the total number of individuals (observations, cases or samples) in the input data matrix. }
  \item{nc}{ the number of individuals remaining in the \sQuote{core} data subset after trimming.  At this stage of a data analysis \code{nc = n}. }
  \item{p}{ the number of variables on which the multivariate operations were based. }
  \item{ifilr}{ flag for \code{gx.md.plot}, set to \code{TRUE}. }
  \item{matnames}{ the row numbers or identifiers and column headings of the input matrix. }
  \item{wts}{ the vector of weights for the \code{n} individuals used to compute the covariance matrix and means.  For a classical, non-robust, estimation all weights are set to \sQuote{1}. }
  \item{mean}{ the vector the clr means for the \code{p} variables. }
  \item{cov}{ the \code{p} by \code{p} clr covariance matrix for the \code{n} by \code{p} data matrix. }
  \item{sd}{ the vector of clr standard deviations for the \code{p} variables. }
  \item{snd}{ the \code{n} by \code{p} matrix of clr standard normal deviates. }
  \item{r}{ the \code{p} by \code{p} matrix of clr Pearson product moment correlation coefficients. }
  \item{eigenvalues}{ the vector of \code{p} eigenvalues of the scaled Pearson correlation matrix for RQ analysis, see Grunsky (2001). }
  \item{econtrib}{ the vector of \code{p} eigenvalues each expressed as a percentage of the sum of the eigenvalues. }
  \item{eigenvectors}{ the \code{n} by \code{p} matrix of \code{eigenvectors}. }
  \item{rload}{ the \code{p} by \code{p} matrix of Principal Component (PC) loadings. }
  \item{rcr}{ the \code{p} by \code{p} matrix containing the percentages of the variability of each variable (rows) expressed in each PC (columns). } 
  \item{rqscore}{ the \code{n} by \code{p} matrix of the n individuals scores on the \code{p} PCs. }
  \item{vcontrib}{ a vector of \code{p} variances of the columns of \code{rqscore}. }
  \item{pvcontrib}{ the vector of \code{p} variances of the columns of \code{rqscore} expressed as percentages.  This is a check on vector \code{econtrib}, the values should be identical. }
  \item{cpvcontrib}{ the vector of \code{p} cumulative sums of \code{pvcontrib}, see above. }
  \item{md}{ the vector of n Mahalanobis distances (MDs) for the \code{n} by \code{p}, now \code{(p-1)}, input matrix. }
  \item{ppm}{ the vector of \code{n} predicted probabilities of population membership, see Garrett (1990). }
  \item{epm}{ the vector of \code{n} empirical Chi-square probabilities for the MDs. }
  \item{nr}{ the number of PCs that have been rotated.  At this stage of a data analysis \code{nr = NULL} in order to control PC plot axis labelling. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any rows in the data matrix with \code{NA}s are removed prior to computions.  In the instance of a compositional data opening transformation \code{NA}s have to be removed prior to undertaking the transformation, see \code{\link{na.omit}}, \code{\link{where.na}} and \code{\link{remove.na}}.  When that procedure is followed the opening transformations may be executed on calling the function, see Examples below.

Warnings are generated when the number of individuals (observations, cases or samples) falls below \code{5p}, and additional warnings when the number of individuals falls below \code{3p}.  At these low ratios of individuals to variables the shape of the \code{p}-space hyperellipsoid is difficult to reliably define, and therefor the results may lack stability.  These limits \code{5p} and \code{3p} are generous, the latter especially so; many statisticians would argue that the number of individuals should not fall below \code{9p}, see Garrett (1993).
}
\references{
Garrett, R.G., 1990. A robust multivariate allocation procedure with applications to geochemical data. In Proc. Colloquium on Statistical Applications in the Earth Sciences (Eds F.P. Agterberg & G.F. Bonham-Carter). Geological Survey of Canada Paper 89-9, pp. 309-318.

Garrett, R.G., 1993. Another cry from the heart. Explore - Assoc. Exploration Geochemists Newsletter, 81:9-14.

Grunsky, E.C., 2001. A program for computing RQ-mode principal components analysis for S-Plus and R. Computers & Geosciences, 27(2):229-235.

Reimann, C., Filzmoser, P., Garrett, R. and Dutter, R., 2008. Statistical Data Analysis Explained: Applied Environmental Statistics with R. John Wiley & Sons, Ltd., 362 p.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{na.omit}}, \code{\link{gx.rqpca.screeplot}}, \code{\link{gx.rqpca.loadplot}}, \code{\link{gx.rqpca.plot}}, \code{\link{gx.rqpca.print}}, \code{\link{gx.md.plot}}, \code{\link{gx.md.print}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}}, \code{\link{gx.rotate}} }
\examples{
## Make test data available
data(sind.mat2open)

## Generate gx.mva object after an clr transform for a PCA
sind.closed <- gx.mva.closed(sind.mat2open)
gx.rqpca.screeplot(sind.closed)
gx.rqpca.plot(sind.closed)
gx.rqpca.loadplot(sind.closed)
## Display saved object with alternate main titles
gx.rqpca.loadplot(sind.closed,
main = "Howarth & Sinding-Larsen\nStream Sediments, clr Transformed Data",
cex.main = 0.8)
gx.rqpca.plot(sind.closed,
main = "Howarth & Sinding-Larsen\nStream Sediments, clr Transformed Data",
cex.main = 0.8)

## Display Mahalanobis distances with alternate main title
gx.md.plot(sind.closed,
main = "Howarth & Sinding-Larsen\nStream Sediments, ilr Transformed Data",
cex.main = 0.8)

## Clean-up
rm(sind.closed)
}
\keyword{ multivariate }

