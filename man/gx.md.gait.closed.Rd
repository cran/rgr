\name{gx.md.gait.closed}
\alias{gx.md.gait.closed}
\title{ Function for Multivariate Graphical Adaptive Interactive Trimming with Compositional Data}
\description{
Function to undertake the GAIT (Graphical Adaptive Interactive Trimming) procedure for multivariate distributions through Chi-square plots of Mahalanobis distances (MDs) as described in Garrett (1988), but for closed compositional, geochemical, data.  To carry out GAIT the function is called repeatedly with the weights from the previous iteration being used as a starting point.  Either a percentage based MVT or a MCD robust start may be used as the first iteration.
}
\usage{
gx.md.gait.closed(xx, wts = NULL, trim = -1, mvtstart = FALSE,
	mcdstart = FALSE, main = deparse(substitute(xx)),
	ifadd = c(0.98, 0.95, 0.9), cexf = 0.6, cex = 0.8, ...)
}
\arguments{
  \item{xx}{ the n by p matrix for which the Mahalanobis distances are required. }
  \item{wts}{ the vector of weights for the n individuals, either 1 or 0. }
  \item{trim}{ the desired trim: trim < 0 - no trim, the default; trim >0 & <1 - fraction, 0 to 1 proportion, of individuals to be trimmed; trim >= 1 - the number of individuals with the highest MDs from the previous iteration to trim. }
  \item{mvtstart}{ set \code{mvtstart = TRUE} for a percentage based MVT (multivariate trim) start. }
  \item{mcdstart}{ set \code{mcdstart = TRUE} for a minimum covariance determinant (mcd) robust start. }
  \item{main}{ an alternative plot title to the default input data matrix name, see Details below. }
  \item{ifadd}{ if probability based fences are to be displayed on the Chi-square plots enter the probabilities here, see Details below.  For no fences set \code{ifadd = NULL}. }
  \item{cexf}{ the scale expansion factor for the Ch-square fence annotation, by default \code{cexf = 0.6}. }
  \item{cex}{ the scale expansion factor for the symbols and text annotation within the \sQuote{frame} of the Chi-square plot, by default \code{cex = 0.8}. } 
  \item{\dots}{ further arguments to be passed to methods concerning the generated plots.  For example, if some colour other than black is required for the plotting characters, specify \code{col = 2} to obtain red (see \code{\link{display.lty}} for the default colour palette).  If it is required to make the plot title or axis labelling smaller, add \code{cex.main = 0.9} or \code{cex.lab = 0.9}, respectively, to reduce the font size by 10\%. }
}
\details{
The variables of the input data matrix must all be expressed in the same units.  An isometric log-ratio (ilr) is undertaken and the transformed data used for the GAIT process.  At the completion of the process the final ilr estimates, including the inverse of the covariance matrix, are transformed to the centred log-ratio (clr) basis.  The vector of means and the inverse of the covariance matrix on a clr basis are required by function \code{\link{gx.mvalloc.closed}}, that is undertaken on a clr basis.

If \code{main} is undefined the name of the matrix object passed to the function is used as the plot title.  This is the recommended procedure as it helps to track the progression of the GAIT.  Alternate plot titles can be defined if the final saved object is passed to \code{\link{gx.md.plot}}.  If no plot title is required set \code{main = " "}, or if a user defined plot title is required it may be defined, e.g., \code{main = "Plot Title Text"}.

By default three fences are placed on the Chi-square plots at probabilities of membership of the current \sQuote{core} data subset, or total data if appropriate, with \code{ifadd = c(0.98, 0.95, 0.9)}.  Alternate probabilities may be defined as best for the display.  If no fences are required set \code{ifadd = NULL}.

The Mahalanobis distance, Chi-square, plot x-axis label is set appropriately to indicated the type of robust start or trim using the value of \code{proc}.
}
\value{
The following are returned as an object to be saved for the next iteration or final use:
  \item{main}{ by default (recommended) the input data matrix name. }
  \item{input}{ the data matrix name, \code{input = deparse(substitute(xx))}, retained to be used by post-processing display functions. }
  \item{matnames}{ the row numbers and column headings of the input matrix. }
  \item{proc}{ the procedure followed for this iteration, used for subsequent Chi-sqaure plot x-axis labelling. }
  \item{wts}{ the vector of weights for the n individuals, either 1 or 0. }
  \item{n}{ the total number of individuals (observations, cases or samples) in the input data matrix. }
  \item{ptrim}{ the percentage, as a fraction, of samples called to be trimmed in this iteration, otherwise \code{ptrim = -1}. }
  \item{mean}{ the p length vector of clr basis means for the \sQuote{core} data following the current GAIT step. }
  \item{cov}{ the p x p clr basis covariance matrix for the \sQuote{core} data following the current GAIT step. }
  \item{cov.inv}{the p x p inverse of the covariance matrix following its transformation to the clr basis from the ilr basis.  For use by function gx.mvalloc.closed. } 
  \item{sd}{ the p length vector of clr basis standard deviations for the \sQuote{core} data following the current GAIT step. }
  \item{md}{ the vector of Mahalanobis distances for all the n individuals following the current GAIT step. }
  \item{ppm}{ the vector of predicted probabilities of membership for all the n individuals following the current GAIT step. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data matrix, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any rows in the data matrix with \code{NA}s are removed prior to computations.  In the instance of a log-ratio, e.g., \code{\link{ilr}}, transformation \code{NA}s have to be removed prior to undertaking the transformation, see \code{\link{na.omit}}, \code{\link{where.na}} and \code{\link{remove.na}}.

Warnings are generated when the number of individuals (observations, cases or samples) falls below \code{5p}, and additional warnings when the number of individuals falls below \code{3p}.  At these low ratios of individuals to variables the shape of the p-space hyperellipsoid is difficult to reliably define, and therefore the results may lack stability.  These limits \code{5p} and \code{3p} are generous, the latter especially so; many statisticians would argue that the number of individuals should not fall below \code{9p}, see Garrett (1993).
}
\references{
Garrett, R.G., 1988. IDEAS - An interactive computer graphics tool to assist the exploration geochemist. In Current Research Part F, Geological Survey of Canada Paper 88-1F, pp. 1-13.

Garrett, R.G., 1993. Another cry from the heart. Explore - Assoc. Exploration Geochemists Newsletter, 81:9-14.

Garrett, R.G., 1989. The Chi-square plot - a tool for multivariate outlier recognition. In Proc. 12th International Geochemical Exploration Symposium, Geochemical Exploration 1987 (Ed. S. Jenness). Journal of Geochemical Exploration, 32(1/3):319-341.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{gx.md.plot}}, \code{\link{gx.md.print}} }
\examples{
## Make test data available
data(sind.mat2open)

## To multivariate trim as in IDEAS, see JGE (1989) 32(1-3):319-341,
## but recognizing that the data are of a closed compositional form
## and using a mcd start, execute:
gx.md.gait.closed(sind.mat2open,ifadd = 0.95)
sind.gait.1 <- gx.md.gait.closed(sind.mat2open, mcdstart = TRUE, 
ifadd = NULL)
sind.gait.2 <- gx.md.gait.closed(sind.mat2open, wts = sind.gait.1$wts,
mvtstart = TRUE, trim = 3, ifadd = 0.9)
sind.gait.3 <- gx.md.gait.closed(sind.mat2open, wts = sind.gait.2$wts, 
trim = 1, ifadd = 0.9)

## Display saved object with alternate main titles and list outliers
gx.md.plot(sind.gait.3, cex.main = 0.8, ifadd = 0.9,
main = "Howarth & Sinding-Larsen\nStream Sediments")
gx.md.print(sind.gait.3, pcut = 0.2)

## Clean-up
rm(sind.gait.1)
rm(sind.gait.2)
rm(sind.gait.3)
}
\keyword{ multivariate }
\keyword{ hplot }
\keyword{ iplot }

