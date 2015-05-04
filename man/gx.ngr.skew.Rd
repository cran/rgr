\name{gx.ngr.skew}
\alias{gx.ngr.skew}
\title{ Estimate the Skewness of a Data Vector }
\description{
Estimates the skew of a data vector for \code{\link{gx.ngr.stats}} to be used by \code{\link{gx.ngr.summary}} to output a NGR Table of summary statistics as a \sQuote{.csv} file.
}
\usage{
gx.ngr.skew(xx)
}
\arguments{
  \item{xx}{ vector for which the skewness is required. }
}
\details{
The formula used, (m3 / m2^1.5) * ((n-1)/n)^1.5, where m3 and m2 are the third and second moments about the mean, respectively, provides an estimate similar to that obtained by Minitab and BMDP. 
}
\value{
  \item{skew}{ the estimate of the skewness of the data vector passed to the function. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector will be removed prior to computation.
}
\author{ Robert G. Garrett }
\examples{
## Make test data available
data(sind)
attach(sind)

## Estimate and display the skewness of the Zn data 
skew <- gx.ngr.skew(Zn)
skew

## Alternately
skew <- gx.ngr.skew(rnorm(0,1,1000))
skew

## Detach test data
detach(sind)
}
\keyword{ univar }

