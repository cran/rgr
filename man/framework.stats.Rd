\name{framework.stats}
\alias{framework.stats}
\title{ Compute Framework/Subset Summary Statistics }
\description{
Function to compute summary statistics for use with function \code{\link{framework.summary}}.
}
\usage{
framework.stats(xx)
}
\arguments{
  \item{xx}{ name of the variable to be processed. }
}
\details{
The function computes summary statistics consisting of the count of valid data, the number of \code{NA}s, the minimum, 2nd, 5th, 10th, 25th (Q1), 50th (median), 75th (Q3), 90th, 95th and 98th percentiles and the maximum. The 95\% confidence interval for the median is computed via the binomial theorem. In addition the Median Absolute Deviation (MAD) and Inter-Quartile Standard Deviation (IQSD) are computed as robust estimates of the standard deviation. Finally, the mean, standard deviation and coefficient of variation as a percentage are computed.
}
\value{
  \item{table}{ a 20-element table is returned, see below. }
  \item{[1]}{ the data/subset (sample) size, N. }
  \item{[2]}{ number of \code{NA}s encountered in the input vector, NNA. }
  \item{[3:13]}{ the data minimum, 2nd, 5th, 10th, 25th (Q1), 50th (median), 75th (Q3), 90th, 95th and 98th percentiles and the maximum. }
  \item{[14:15]}{ the lower and upper 95\% confidence bounds for the median. } 
  \item{[16]}{ the Median Absolute Deviation (MAD). }
  \item{[17]}{ the Inter-Quartile Standard Deviation (IQSD). }
  \item{[18]}{ the data (sample) Mean. } 
  \item{[19]}{ the data (sample) Standard Deviation (SD). }
  \item{[20]}{ the Coefficient of Variation as a percentage (CV\%). }
}
\note{  
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data vector, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are counted and then removed prior to computing the summary statistics.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}} }
\examples{
## Make test data available
data(kola.c)
attach(kola.c)

## Computes summary statistics for the Cu data
fs <- framework.stats(Cu)
fs

## Computes summary statistics for Finnish subset of the Cu data
fs <- framework.stats(Cu[COUNTRY == "FIN"])
fs

## Detach test data
detach(kola.c)
}
\keyword{ univar }

