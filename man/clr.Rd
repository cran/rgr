\name{clr}
\alias{clr}
\title{ Centred Log-Ratio (clr) transformation }
\description{
Undertakes a centred log-ratio transformation to remove the effects of closure in a data matrix.
}
\usage{
clr(xx, ifclose = FALSE, ifwarn = TRUE)
}
\arguments{
  \item{xx}{ a n by p matrix to be log centred.  It is essential that a single unit of measurement is used.  Thus it may be required to convert, for example, determinations in percent to ppm (mg/kg) so that all measurements are in ppm prior to executing this function.  Natural logarithms are used. }
  \item{ifclose}{ if it is required to close a data set prior to transformation set \code{ifclose = TRUE}. }
  \item{ifwarn}{ by default \code{ifwarn = TRUE} which generates a reminder/warning that when carrying out a centred log-ratio transformation all the data must be in the same measurement units.  The message can be suppressed by setting \code{ifwarn = FALSE}. }
}
\details{
Most analytical chemical data for major, minor and trace elements are of a closed form, i.e. for a sample they sum to a constant, whether it be percent, ppm (mg/kg), or some other units.  It does not matter that only some components contributing to the constant sum are present in the matrix, the data are closed.  As a result, as some elements increase in concentration others must decrease, this leads to correlation measures and graphical presentations that do not reflect the true underlying relationships.  A centred log-ratio is one procedure for removing closure effects, others are additive log-ratios (\code{alr}) and isometric log-ratios (\code{ilr}).
}
\value{
  \item{x}{ a n by p matrix of log-centred values. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any rows containing \code{NA}s in the data matrix are removed prior to undertaking the transformation.

The \code{clr} transform is suitable for the study of correlation coefficients and subsequent multivariate data analyses.  However, for the calculation of Mahalanobis distances, which require matrix inversion, \code{\link{ilr}} should be used.  Furthermore, in some cases it is preferable to use an \code{ilr} transform prior to undertaking a Principal Component or Factor Analysis, however, a \code{clr} transform is often sufficient.

The \code{ifclose} option can be useful if a petrochemical ternary system is under investigation.  A data subset for a ternary system may be closed and transformed for investigation in x-y plots and comparison with the inferences that may be drawn from a classical ternary diagram display.  Ternary plots are not included in this release of 'rgr', their use is discouraged as they do not reveal the true inter-component relationships.  However, their use as classification tools is acknowledged where a user's data may be compared to data for known rock types and processes, etc.  R users interested in ternary and classification diagrams rather than exploratory data analysis should investigate GCDkit (ver 2.3, R 2.7.0 2008/05/11) by Janousek, Farrow, Erban and Smid.  See also Janousek et al. (2006). 
}
\references{
Aitchison, J., 1984. The statistical analysis of geochemical compositions. Mathematical Geology, 16(6):531-564.

Aitchison, J., 1986. Ths Statistical Analysis of Compositional data. Chapman and Hall, London, U.K., 416 p.

Aitchison, J. and Egozcue, J.J., 2005. Compositional data analysis; where are we and where should we be heading. Mathematical Geology, 37(7):829-850.

Buccianti, A., Mateu-Figueras, G, and Pawlowsky-Glahn, V. (eds.), 2006. Compositional data analysis in the geosciences: from theory to practice. The Geological Society Publishing House, Bath, U.K. Special Publication 264, 224 p.

Janousek, V., Farrow, C.M. and Erban, V., 2006. Interpretation of whole-rock geochemical data in igneous geochemistry introducing Geochemical Data Toolkit (GCDkit). Journal of Petrology, 47(6):1255-1259.

Reimann, C., Filzmoser, P., Garrett, R. and Dutter, R., 2008. Statistical Data Analysis Explained: Applied Environmental Statistics with R. Wiley, 362 p.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{clr}}, \code{\link{ilr}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}} }
\examples{
## Make test data available
data(sind)
sind.mat <- as.matrix(sind[, -c(1:3)])

## Undertake clr transform, note necessity
## of converting percent Fe to mg/kg
sind.mat[, 2] <- sind.mat[, 2] * 10000
temp <- clr(sind.mat)
temp

## Clean-up and detach test data
rm(sind.mat)
rm(temp)
}
\keyword{ multivariate }
\keyword{ manip }

