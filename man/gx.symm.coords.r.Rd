\name{gx.symm.coords.r}
\alias{gx.symm.coords.r}
\title{ Displays a Matrix of Correlation Coefficients based on Symmetric Balances }
\description{
Computes and displays a \code{p} by \code{p} matrix of correlation coefficients based on symmetric balances for a \code{n} by \code{p} matrix or data frame of compositional data.  Computation of the correlation coefficients is by any valid R method, by default \code{"spearman"}.  The symmetric balance coefficients are displayed in the upper triangle, and for comparison, the correlation coefficients of the input data are displayed in the lower triangle.  If \code{"pearson"} coefficients are required the option of a log transformation for the data is provided.
}
\usage{
gx.symm.coords.r(x, log = FALSE, method = "spearman" )
}
\arguments{
  \item{x}{ \code{n} by \code{p} matrix or data frame of compositional data for which the correlation coefficients will be computed. }
  \item{log}{to compute the non-symmetric balance coefficients with a logarithmic transformation and plot scaling, set \code{log = TRUE}. }
  \item{method}{the valid R method for computation of the correlation coefficients, the default is \code{"spearman"}. }
}
\value{
  \item{r.sbs}{ the \code{p} by \code{p} matrix of correlation coefficients. }
}
\note{
For compositional data analysis all the data must be in the same measurement units.

The \code{"spearman"} coefficient is preferred for EDA as any systematic monotonic variation in the data is of interest, and may be worthy of further investigation.  As \code{"spearman"} coefficients are based on ranks, any monotonic data transformation, e.g., logarithmic, has no impact on the results.  This is not the case for \code{"pearson"} coefficients.
}
\references{
Garrett, R.G., Reimann, C., Hron, K., Kynclova, P. and Filzmoser, P., 2017. Finally, a correlation coefficient that tells the geochemical truth. Explore - Assoc. Applied Geochemists Newsletter, 176:1-10. 

Reimann, C., Filzmoser, P., Hron, K., Kynclova, P. and Garrett, R.G., 2017. Correlation Analysis for Compositional (Environmental) Data. Science of the Total Environment, 607-608:965-971.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.symm.coords}}, \code{\link{gx.symm.coords.plot}} }
\examples{
## Make test data available
data(nockolds)

## Compute and display correlation coefficients for the nockolds data
gx.symm.coords.r(nockolds)

## Save the matrix for further use
save <- gx.symm.coords.r(nockolds)

## Compute and display correlation coefficients for the nockolds data
## based on pearson product moment correlation coefficients with a
## logarithmic transform for the non-symmetric balance coefficients
gx.symm.coords.r(nockolds, method = "pearson", log = TRUE)

## Clean-up
rm(nockolds)
rm(save)
}
\keyword{ multivariate }

