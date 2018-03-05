\name{gx.ilr}
\alias{gx.ilr}
\title{ Isometric Log-Ratio (ilr) transformation }
\description{
Undertakes an isometric log-ratio transformation to remove the effects of closure in a data frame or matrix on a single part to support univariate investigations. }
\usage{
gx.ilr(xx, v1, ifwarn = FALSE)
}
\arguments{
  \item{xx}{ a \code{n} by \code{p} matrix from which a single part, \code{v1}, is to be isometrically log-ratio transformed.  It is essential that a single unit of measurement is used for all parts.  Thus it may be required to convert, for example, determinations in percent to ppm (mg/kg) so that all measurements are in ppm prior to executing this function.  Natural logarithms are used. }
  \item{v1}{ the column index of the part to be ilr transformed. }
  \item{ifwarn}{ set to \code{FALSE} to suppress the warningdisplay concerning all the data/parts being in the same units. The message can be displayed by setting \code{ifwarn = TRUE}. }
}
\details{
Most analytical chemical data for major, minor and trace elements are of a closed form, i.e. for a physical individual sample they sum to a constant, whether it be percent, ppm (mg/kg), or some other units.  It does not matter that only some components contributing to the constant sum are present in the matrix, the data are closed.  As a result, as some elements increase in concentration others must decrease, this leads to correlation measures that do not reflect the true underlying relationships, see \code{\link{gx.symm.coords.r}}.
}
\value{
  \item{z}{ a vector \code{z} of length \code{n} of isometric log-ratios for part \code{v1}.  }
}
\note{
\code{xx} may be a data frame or matrix, but must contains only parts, compositions, for the data set under study.  This function should only be used for univariate investigations of a single part \code{v1}, it is inappropriate to use this transformation for multivariate studies, in which case use functions \code{\link{ilr}} or \code{\link{clr}} should be employed.

Note that the vector \code{z} is numerically equal to the clr transform for \code{v1} in the data matrix \code{xx} times a constant \code{sqrt(p/(p-1))}, where \code{p} is the mumber of parts in the matrix \code{xx}.

Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any rows containing \code{NA}s in the data will be removed prior to undertaking the transformation.

}
\references{
Aitchison, J. and Egozcue, J.J., 2005. Compositional data analysis; where are we and where should we be heading. Mathematical Geology, 37(7):829-850.

Filzmoser, P., Hron, K. and Reimann, C., 2009. Principal component analysis for compositional data with outliers. Environmetrics, 20(6):621-633.

Filzmoser, P., Hron, K. and Reimann, C., 2009. Univariate statistical analysis of environmental (compositional) data - problems and possibilities. Science of the Total Environment, 407:6100-6108.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ilr}}, \code{\link{ltdl.fix.df}}  }
\examples{
## Make test data sind available
data(sind.mat2open)

## Undertake ilr transform
temp <- gx.ilr(sind.mat2open, 5)
temp

## Clean-up
rm(temp)
}
\keyword{ multivariate }
\keyword{ manip }

