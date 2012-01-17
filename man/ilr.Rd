\name{ilr}
\alias{ilr}
\title{ Isometric Log-Ratio (ilr) transformation }
\description{
Undertakes an isometric log-ratio transformation to remove the effects of closure in a data matrix. }
\usage{
ilr(xx, ifclose = FALSE, ifwarn = TRUE)
}
\arguments{
  \item{xx}{ a n by p matrix to be isometrically log-ratio transformed.  It is essential that a single unit of measurement is used.  Thus it may be required to convert, for example, determinations in percent to ppm (mg/kg) so that all measurements are in ppm prior to executing this function.  Natural logarithms are used. }
  \item{ifclose}{ if it is required to close a data set prior to transformation set \code{ifclose = TRUE}. }
  \item{ifwarn}{ by default \code{ifwarn = TRUE} which generates a reminder/warning that when carrying out a centred log-ratio transformation all the data must be in the same measurement units.  The message can be suppressed by setting \code{ifwarn = FALSE}. }
}
\details{
Most analytical chemical data for major, minor and trace elements are of a closed form, i.e. for a sample they sum to a constant, whether it be percent, ppm (mg/kg), or some other units.  It does not matter that only some components contributing to the constant sum are present in the matrix, the data are closed.  As a result, as some elements increase in concentration others must decrease, this leads to correlation measures and graphical presentations that do not reflect the true underlying relationships.  However, isometrically transformed data are not suitable for univariate EDA inspection as the new synthetic variables bear a complex relationship to the original measurements. Other procedures for removing closure effects are additive log-ratios (\code{alr}) and centred log-ratios (\code{clr}).
}
\value{
  \item{x}{ a n by (p-1) matrix of isometric log-ratio values.  The names of the new (p-1) synthetic variables, \code{iso1} through to \code{isop}, where the \code{p} in \code{isop} equals \code{p-1}, are entered as column names in the matrix. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data matrix, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any rows containing \code{NA}s in the data matrix are removed prior to undertaking the transformation.

The \code{ilr} transform is recommended for the calculation of Mahalanobis distances, a procedure which requires matrix inversion.  When  a Principal Component or Factor Analysis is required use of the \code{ilr} transform may be preferrable, see also the notes in \code{\link{clr}}.  In that instance back transformation from the isometrically transformed variables to the original variables is required.  Interested R users should refer to the papers by Filzmoser et al. (see below).
}
\references{
Aitchison, J. and Egozcue, J.J., 2005. Compositional data analysis; where are we and where should we be heading. Mathematical Geology, 37(7):829-850.

Buccianti, A., Mateu-Figueras, G, and Pawlowsky-Glahn, V. (eds.), 2006. Compositional data analysis in the geosciences: from theory to practice. The Geological Society Publishing House, Bath, U.K. Special Publication 264, 224 p.

Filzmoser, P. and Hron, K., 2008. Outlier detection for compositional data using robust methods. Mathematical Geosciences, 40(3):234-248.

Filzmoser, P., Hron, K. and Reimann, C., 2009. Principal component analysis for compositional data with outliers. Environmetrics, 20(6):621-633.

Filzmoser, P., Hron, K., Reimann, C. and Garrett, R.G., 2009. Robust factor analysis for compositional data. Computers & Geosciences, 35(9):1854-1861.
}
\author{ Peter Filzmoser and Karel Hron, with additions by Robert G. Garrett }
\seealso{ \code{\link{alr}}, \code{\link{clr}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}} }
\examples{
## Make test data sind available
data(sind.mat2open)

## Undertake ilr transform
temp <- ilr(sind.mat2open)
temp

## Clean-up
rm(temp)
}
\keyword{ multivariate }
\keyword{ manip }

