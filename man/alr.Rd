\name{alr}
\alias{alr}
\title{ Arithmetic Log-Ratio (alr) transformation }
\description{
Undertakes an arithmetic log-ratio transformation to remove the effects of closure in a data matrix. }
\usage{
alr(xx, j = NULL, ifclose = FALSE, ifwarn = TRUE)
}
\arguments{
  \item{xx}{ a n by p matrix to be arithmetically log centred.  It is essential that a single unit of measurement is used.  Thus it may be required to convert, for example, determinations in percent to ppm (mg/kg) so that all measurements are in ppm prior to executing this function.  Natural logarithms are used. }
  \item{j}{ the index number of the element [1:p] to be used as the divisor, \code{j}, must be defined, there is no default index. }
  \item{ifclose}{ if it is required to close a data set prior to transformation set \code{ifclose = TRUE}. }
  \item{ifwarn}{ by default \code{ifwarn = TRUE} which generates a reminder/warning that when carrying out a arithmetic log-ratio transformation all the data must be in the same measurement units.  The message can be suppressed by setting \code{ifwarn = FALSE}. }
}
\details{
Most analytical chemical data for major, minor and trace elements are of a closed form, i.e. for a sample they sum to a constant, whether it be percent, ppm (mg/kg), or some other units.  It does not matter that only some components contributing to the constant sum are present in the matrix, the data are closed.  As a result, as some elements increase in concentration others must decrease, this leads to correlation measures and graphical presentations that do not reflect the true underlying relationships.  An arithmetic log-ratio is one procedure for removing closure effects, others are centred log-ratios (\code{clr}) and isometric log-ratios (\code{ilr}).

Care should be taken in selecting the variable, index = \code{j}, for use as the divisor.  Variables lacking sufficient significant figures in their quantification, or variables measured at close to their measurement detection limits, should be avoided.

It is worth noting that when the \code{alr} transform is undertaken with a geochemically conservative element selected as the divisor and two elements are then displayed in an x-y plot the result is a Pearce Element Ratio plot (Pearce, 1968) with log scaling.
}
\value{
  \item{x}{ a n by (p-1) matrix of arithmetically log-centred values, the j-th column of the matrix being dropped. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data matrix, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any rows containing \code{NA}s in the data matrix are removed prior to undertaking the transformation.
}
\references{
Aitchison, J., 1984. The statistical analysis of geochemical compositions. Mathematical Geology, 16(6):531-564.

Aitchison, J., 1986. Ths Statistical Analysis of Compositional data. Chapman and Hall, London, U.K., 416 p.

Aitchison, J. and Egozcue, J.J., 2005. Compositional data analysis; where are we and where should we be heading. Mathematical Geology, 37(7):829-850.

Buccianti, A., Mateu-Figueras, G, and Pawlowsky-Glahn, V. (eds.), 2006. Compositional data analysis in the geosciences: from theory to practice. The Geological Society Publishing House, Bath, U.K. Special Publication 264, 224 p.

Pearce, T.H., 1968. A contribution to the theory of variation diagrams. Contributions to Mineralogy and Petrolgy, 19(2):142-157.

Reimann, C., Filzmoser, P., Garrett, R. and Dutter, R., 2008. Statistical Data Analysis Explained: Applied Environmental Statistics with R. Wiley, 362 p.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{clr}}, \code{\link{ilr}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}} }
\examples{
## Make test data available
data(sind)
sind.mat <- as.matrix(sind[, -c(1:3)])

## Undertake alr transform, use Pb [j = 6 ] as the divisor,
## note necessity of converting percent Fe to mg/kg
sind.mat[, 2] <- sind.mat[, 2] * 10000
temp <- alr(sind.mat, 6)
temp

## Clean-up
rm(sind.mat)
rm(temp)
}
\keyword{ multivariate }
\keyword{ manip }

