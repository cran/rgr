\name{alts2dups}
\alias{alts2dups}
\title{ Create a Matrix of Duplicate Pairs from Sequential Data }
\description{
Function to take data stored as stacked records or alternating rows of records for \code{ndup} duplicate pairs and generate a matrix with \code{ndup} rows and two columns for the duplicate data, for further details see \code{x} in Arguments below.  The function returns a matrix for processing.
}
\usage{
alts2dups(x, ifalt = FALSE)
}
\arguments{
  \item{x}{ a column vector from a matrix or data frame, \code{x[1], ..., x[2*n]}. The default is that the first n members of the vector are the first measurements and the second n members are the duplicate measurements.  If the measurements alternate, i.e. duplicate pair 1 measurement 1 followed by measurement 2, etc., set \code{ifalt = TRUE}. }
  \item{ifalt}{ set \code{ifalt = TRUE} to accommodate alternating sets of paired observations. }
}
\details{
Function provides an easy procedure with function \code{plot} to present duplicate data as an x-y plot.  The matrix generated can be passed directly to \code{plot} in its call and the matrix column headings synthesized from the input variable name are used as plot axis titles.
}
\value{
  \item{xx}{ a \code{ndup} by \code{2} matrix containing the duplicate pairs.  Row names are sequenced \code{1:ndup} and the two column names are generated from the input variable name, \code{x} with \code{.1} or \code{.2} appended, respectively. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.
}
\author{ Robert G. Garrett }
\examples{
## Make test data available
data(ad.test)
attach(ad.test)

## Save matrix of duplicate pairs
test.save <- alts2dups(Cu, ifalt = TRUE)

## Plot analytical duplicate analyses
plot(alts2dups(Cu, ifalt = TRUE), log = "xy")

## Clean-up and detach test data
rm(test.save)
detach(ad.test)
}
\keyword{ misc }
\keyword{ manip }
