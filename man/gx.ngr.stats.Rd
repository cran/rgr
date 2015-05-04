\name{gx.ngr.stats}
\alias{gx.ngr.stats}
\title{ Computes Summary Statistics for a NGR Report Table }
\description{
Builds a vector of summary statistics for \code{\link{gx.ngr.summary}} to output a NGR Report table of summary statistics as a \sQuote{.csv} file from estimates made by \code{\link{gx.stats}} and \code{\link{gx.ngr.skew}}. 
}
\usage{
gx.ngr.stats(xx)
}
\arguments{
  \item{xx}{ vector for which NGR Report Table summary statistics are required. }
}
\value{
  \item{table}{ 
the computed summary statistics to be used in function \code{\link{gx.ngr.summary}}.  The list returned, \code{table}, is a 29-element vector, see below: }
  \item{[1]}{ the sample size, N. }
  \item{[2]}{ the number of NAs in the input vector. }
  \item{[3]}{ the data (sample) Mean. }
  \item{[4]}{ the data (sample) Standard Deviation (SD). }
  \item{[5]}{ the data (sample) Skew. }
  \item{[6]}{ the Coefficient of Variation as a percentage (CV\%). }
  \item{[7]}{ the data (sample) Geometric Mean. }
  \item{[8]}{ the data (sample) Median. }
  \item{[9]}{ the data (sample) Median Absolute Deviation (MAD). }
  \item{[10]}{ the Robust Coefficient of Variation as a percentage (RCV\%). } 
  \item{[11:20]}{ the minimum value, and the 1st, 2nd, 5th, 10th, 20th, 25th (Q1), 30th, 40th and 50th (Q2) percentiles. }
  \item{[21:29]}{ the 60th, 70th, 75th (Q3), 80th  90th, 95th, 98th and 99th percentiles and the maximum value. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector will be removed prior to computation in function \code{\link{gx.stats}}.  Display of the number of \code{NAs} found by function \code{remove.na} is suppressed in \code{remove.na} as the information is included in the display from this function.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.summary1}}, \code{\link{gx.summary}}, \code{\link{gx.stats}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{gx.summary2}} }
\examples{
## Make test data available
data(sind)
attach(sind)

## Generate and display the results for Zn 
table <- gx.ngr.stats(Zn)
table

## Detach test data
detach(sind)
}
\keyword{ univar }

