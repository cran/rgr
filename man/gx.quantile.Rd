\name{gx.quantile}
\alias{gx.quantile}
\title{ Estimate the Quantile for a Specified Fractile }
\description{
Estimates and displays the quantile for a specified fractile of a data set by linear interpolation from the ranked data.  If the function is run as \code{temp <- gx.quantile(xx, f)} the quantile is not displayed, but retained in \code{temp} for subsequent use or display.
}
\usage{
gx.quantile(xx, f, display = TRUE)
}
\arguments{
  \item{xx}{ the data set for which the quantile is to be estimated. }
  \item{f}{ the fractile for which the quantile is required. }
  \item{display}{ the default is to display the fractile and estimated quantile on the current device. If no display is required, set \code{display = FALSE}. }
}
\value{
  \item{q}{ the estimated quantile. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data vector, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are removed prior to preparing the boxplots.

The result is an approximation, and the result from the \code{quantile} function will likely differ by some small amount.
}
\author{ Based on a script shared on S-News by Nick Ellis, April 2002 }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{gx.fractile}} }
\examples{
## Make test data available
data(kola.o)
attach(kola.o)

## Estimate the 80th percentile, f = 0.8
gx.quantile(As, 0.8)
temp <- gx.quantile(As, 0.8)
temp

## Clean-up and detach test data
rm(temp)
detach(kola.o) 
}
\keyword{ univar }

