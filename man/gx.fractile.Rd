\name{gx.fractile}
\alias{gx.fractile}
\title{  Estimate the Fractile for a Specified Quantile }
\description{
Estimates the fractile for a specified quantile of a data set by linear interpolation from the ranked data.  If the function is run as \code{temp <- gx.fractile(xx, q)} the fractile is not displayed, but retained in \code{temp} for subsequent use or display.
}
\usage{
gx.fractile(xx, q, display = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xx}{ the data set for which the quantile is to be estimated. }
  \item{q}{ the fractile (value) for which the quantile is required, must be within the range of \code{xx}. }
  \item{display}{ the default is to display the quantile and estimated fractile on the current device. If no display is required, set \code{display = FALSE}. }
}
\value{
  \item{f}{ the estimated fractile. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are removed prior to preparing the boxplots.
}
\author{ Based on a script shared on S-News by Nick Ellis, April 2002 }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{gx.quantile}} }
\examples{
## Make test data available
data(kola.o)
attach(kola.o)

## Estimate the fractile for 20 mg/kg As
gx.fractile(As, 20)
temp <- gx.fractile(As, 20)
temp

## Clean-up and detach test data
rm(temp)
detach(kola.o) 
}
\keyword{ univar }

