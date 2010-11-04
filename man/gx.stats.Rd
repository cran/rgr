\name{gx.stats}
\alias{gx.stats}
\title{ Compute Summary Statistics }
\description{
Function to compute summary statistics for a 'one-page' report and display in \code{\link{inset}}.  Function may be used stand-alone.
}
\usage{
gx.stats(xx, xlab = deparse(substitute(xx)), display = TRUE)
}
\arguments{
  \item{xx}{ name of the variable to be processed. }
  \item{xlab}{ a title for the table. It is often desirable to replace the default table title of the input variable name text string with a more informative title, e.g., \code{xlab = "Cu (mg/kg) in <2 mm O-horizon soil"}. }
  \item{display}{ if \code{display = TRUE} the summary statistics are displayed on the current device.  If \code{display = FALSE} output is suppressed.}
}
\details{
The summary statistics comprise the data minimum, maximum and percentile values, robust estimates of standard deviation, the Median Absolute Deviation (MAD) and the Inter Quartile Standard Deviation (IQSD), and the mean, variance, standard deviation (SD) and coefficient of variation (CV\%).
}
\value{
  \item{table}{ 
the computed summary statistics to be used in function \code{\link{inset}}.  The list returned, \code{table}, is a 26-element vector, see below. }
 \item{[1:10]}{ the minimum value, and the 1st, 2nd, 5th, 10th, 20th, 25th (Q1), 30th, 40 and 50th(Q2) percentiles }
 \item{[11:19]}{ the 60th, 70th, 75th(Q3), 80th  90th, 95th, 98th and 99th percentiles and the maximum value }
 \item{[20]}{ the sample size, N }
 \item{[21]}{ the Median Absolute Deviation (MAD) }
 \item{[22]}{ The Inter-Quartile Standard Deviation (IQSD) }
 \item{[23]}{ the data (sample) Mean }
 \item{[24]}{ the data (sample) Variance }
 \item{[25]}{ the data (sample) Standard Deviation (SD) }
 \item{[26]}{ the Coefficient of Variation as a percentage (CV\%) }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data vector, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are removed prior to computation. 
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}} } 
\examples{
## Make test data available
data(kola.o)
attach(kola.o)

## Generates an initial display
gx.stats(Cu)

## Provides a more appropriate labelled display
gx.stats(Cu, xlab = "Cu (mg/kg) in <2 mm O-horizon soil")

## Detach test data
detach(kola.o)

}
\keyword{ univar }

