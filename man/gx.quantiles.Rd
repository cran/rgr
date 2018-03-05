\name{gx.quantiles}
\alias{gx.quantiles}
\title{ Estimate the Common Quantiles }
\description{
Estimates and displays the common quantiles. i.e. quartiles, quintiles, deciles, and for upper and lower extreme percentiles, for a numeric variable.  For any other quantile use function \code{\link{gx.quantile}}.
}
\usage{
gx.quantiles(x, xname = deparse(substitute(x)))
}
\arguments{
  \item{x}{ the data set for which the quantiles are to be estimated. }
  \item{xname}{ by default the character string for \code{x} is used for the display.  An alternate title can be displayed with \code{xname = "text string"}, see Example. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are removed prior to estimation.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{gx.fractile}} }
\examples{
## Make test data available
data(kola.o)
attach(kola.o)

## Estimate the common quantiles
gx.quantiles(As, "As (mg/kg) in Kola O-horizon soil")

## Clean-up and detach test data
detach(kola.o) 
}
\keyword{ univar }

