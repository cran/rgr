\name{gx.spearman}
\alias{gx.spearman}
\title{ Display Spearman Correlation Coefficients and their Significances }
\description{
The function computes Spearman rank correlation coefficients and places them in the upper triangle of a printed matrix displayed on the current device, the probabilities that the coefficients are not due to chance (Ho: Coefficient = 0) are printed in the lower triangle.  The diagonal is filled with NAs to visually split the two triangles.
}
\usage{
gx.spearman(xx, ifclr = FALSE, ifwarn = TRUE)
}
\arguments{
  \item{xx}{ a matrix of numeric data. }
  \item{ifclr}{ if \code{ifclr = TRUE} the data are Centred Log-Ratio transformed prior to the computation of the Pearson Coefficients.  The default is no transformation. }
  \item{ifwarn}{ by default \code{ifwarn = TRUE} which generates a reminder/warning that when carrying out a centred log-ratio transformation all the data must be in the same measurement units.  The message can be suppressed by setting \code{ifwarn = FALSE}. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data vector, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are removed prior to computation.

For closed compositional data sets, i.e. geochemimical analyses, it is highly recommended that \code{ifclr} be set to \code{TRUE} to remove the effects of closure and display the true inter-element variability.  When carrying out a centred log-ratio transformation it is essential that the data are all in the same measurement units, and by default a reminder/warning is display if the data are centred log-ratio transformed, see \code{ifwarn} above.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{clr}} }
\examples{
## Make test data available
data(sind)
attach(sind)
sind.mat <- as.matrix(sind[, -c(1:3)])

## Compute Spearman correlation coefficients
gx.pearson(sind.mat)

## Compute Spearmann correlation coefficients following
## a centred log ratio transformation, note necessity
## of converting percent Fe to mg/kg
sind.mat[, 2] <- sind.mat[, 2] * 10000
gx.pearson(sind.mat, ifclr = TRUE)

## Clean-up and detach test data
rm(sind.mat)
detach(sind)  
}
\keyword{ multivariate }
\keyword{ nonparametric }
\keyword{ htest }

