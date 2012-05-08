\name{gx.pearson}
\alias{gx.pearson}
\title{ Display Pearson Correlation Coefficients and their Significances }
\description{
The function computes Pearson product moment correlation coefficients and places them in the upper triangle of a printed matrix displayed on the current device, the probabilities that the coefficients are not due to chance (Ho: Coefficient = 0) are printed in the lower triangle.  The diagonal is filled with NAs to visually split the two triangles.
}
\usage{
gx.pearson(xx, log = FALSE, ifclr = FALSE, ifwarn = TRUE)
}
\arguments{
  \item{xx}{ a matrix of numeric data. }
  \item{log}{ if \code{log = TRUE} the data are log10 transformed prior to computation of the Pearson coefficients.  The default is no transformation. } 
  \item{ifclr}{ if \code{ifclr = TRUE} the data are Centred Log-Ratio transformed prior to the computation of the Pearson Coefficients.  The default is no transformation. }
  \item{ifwarn}{ by default \code{ifwarn = TRUE} which generates a reminder/warning that when carrying out a centred log-ratio transformation all the data must be in the same measurement units.  The message can be suppressed by setting \code{ifwarn = FALSE}. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any data vectors (rows) containing \code{NA}s are removed prior to computation.

For working with compositional data sets functions \code{\link{gx.vm}} and \code{\link{gx.sm}} are recommended.

This function is not recommended for use with closed compositional data sets, i.e. geochemical analyses, unless correlations are sought between a non-compositional variable and individual compositional variables.  If it is used with compositional data, it is highly recommended that \code{ifclr} be set to \code{TRUE} to remove the effects of closure and display the \sQuote{true} inter-element variability.  However, different groups of elements, subsets, of a data set will yield different inter-element correlations for the same pair of elements due to the nature of the \code{clr} transform.  When carrying out a centred log-ratio transformation it is essential that the data are all in the same measurement units, and by default a reminder/warning is display if the data are centred log-ratio transformed, see \code{ifwarn} above.

For working with compositional data sets functions \code{\link{gx.vm}} and \code{\link{gx.sm}} are recommended.  For visual displays see \code{\link{gx.pairs4parts}} and \code{\link{gx.plot2parts}}.

When a centred log-ratio transformation is undertaken the \code{log} \sQuote{switch} is ignored.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{clr}}, \code{\link{sind.mat2open}} }
\examples{
## Make test data available
data(sind.mat2open)

## Compute Pearson correlation coefficients
gx.pearson(sind.mat2open)

## Compute Pearson correlation coefficients following
## a logarithmic transformation
gx.pearson(sind.mat2open, log = TRUE)

## Compute Pearson correlation coefficients following
## a centred log-ratio transformation
gx.pearson(sind.mat2open, ifclr = TRUE)
}
\keyword{ multivariate }
\keyword{ htest }

