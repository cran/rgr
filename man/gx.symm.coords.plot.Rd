\name{gx.symm.coords.plot}
\alias{gx.symm.coords.plot}
\title{ Display a X-Y Plot, Scatterplot, of Symmetric Balances }
\description{
Displays a scatterplot based on symmetric balances for two parts, \code{v1} and \code{v2}, from a \code{n} by \code{p} matrix or data frame of compositional data, and their correlation coefficient by the selected method, default \code{spearman}.  Optionally the display in Garrett et al. (2017) may be generated, in which case the correlation coefficients estimated by any valid R method, by default \code{"spearman"} are displayed above the plots.  By default the parts are displayed, left plot, with log-scaling.  To plot without log-scaling, set \code{log = FALSE}, in which case the correlation coefficient is estimated without a logarithmic transformation.  If \code{"pearson"} coefficients are required the option of a log transformation for the data is provided.
}
\usage{
gx.symm.coords.plot(x, v1, v2, log = TRUE, method = "spearman", example = " ", ... )
}
\arguments{
  \item{x}{ \code{n} by \code{p} matrix or data frame of compositional data for which a scatterplot will be displayed for two parts. }
  \item{v1}{ the column index for the part to be displayed on the x-axis. }
  \item{v2}{ the column index for the part to be displayed on the y-axis. }
  \item{log}{to compute the non-symmetric balance coefficients with a logarithmic transformation, set \code{log = TRUE}. }
  \item{method}{the valid R method for computation of the correlation coefficient between the symmetric balances for \code{v1} and \code{v2}, the default is \code{"spearman"}. }
  \item{example}{ to reproduce the display in Garrett et al. (2017) set \code{example = "Explore"} or \code{"explore"}. }
  \item{\dots}{ further arguments to be passed to methods concerning the plot. } 
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
\seealso{ \code{\link{gx.symm.coords}}, \code{\link{gx.symm.coords.r}} }
\examples{
## Make test data available
data(nockolds)

## Display symmetric balances scatterplot for variable 1, Si, and 2, Al, for the nockolds data
gx.symm.coords.plot(nockolds, 1, 2)

## Display the example from Garrett et al. (2017)
gx.symm.coords.plot(nockolds, 1, 2, example = "explore")

## Clean-up
rm(nockolds)
}
\keyword{ multivariate }

