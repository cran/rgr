\name{gx.stats}
\alias{gx.stats}
\title{ Function to Compute and Display Summary Statistics }
\description{
Function to compute summary statistics for a 'one-page' report and display in \code{\link{inset}}.  Function may be used stand-alone, and is used as an \sQuote{engine} for the \code{gx.summary.*} series of functions
}
\usage{
gx.stats(xx, xlab = deparse(substitute(xx)), display = TRUE,
	iftell = TRUE)
}
\arguments{
  \item{xx}{ name of the variable to be processed. }
  \item{xlab}{ by default the character string for \code{xx} is used for the table title.  An alternate title can be displayed with \code{xlab = "text string"}, see Examples. }
  \item{display}{ if \code{display = TRUE} the summary statistics are displayed on the current device.  If \code{display = FALSE} output is suppressed.}
  \item{iftell}{ by default the NA count is displayed by \code{na.remove} prior to the table of results from this function.  When the function is used as a \sQuote{stats} engine the \code{NA} count display may be suppressed by the calling function when the NA count is to be displayed by that calling function. }
}
\details{
The summary statistics comprise the data minimum, maximum and percentile values, robust estimates of standard deviation, the Median Absolute Deviation (MAD) and the Inter Quartile Standard Deviation (IQSD), and the mean, variance, standard deviation (SD), coefficient of variation (CV\%), and the 95\% confidence bounds on the median.  When the minimum data value is \code{> 0} summary statistics are computed after a log10 data transformation and exported back to the calling function.
}
\value{
  \item{stats}{ 
the computed summary statistics to be used in function \code{\link{inset}}, and by \cr\code{gx.summary.*} functions.  The list returned, \code{stats}, is a 32-element vector, see below: }
  \item{[1:10]}{ the minimum value, and the 1st, 2nd, 5th, 10th, 20th, 25th (Q1), 30th, 40th and 50th (Q2) percentiles. }
  \item{[11:19]}{ the 60th, 70th, 75th (Q3), 80th  90th, 95th, 98th and 99th percentiles and the maximum value. }
  \item{[20]}{ the sample size, N. }
  \item{[21]}{ the Median Absolute Deviation (MAD). }
  \item{[22]}{ the Inter-Quartile Standard Deviation (IQSD). }
  \item{[23]}{ the data (sample) Mean. }
  \item{[24]}{ the data (sample) Variance. }
  \item{[25]}{ the data (sample) Standard Deviation (SD). }
  \item{[26]}{ the Coefficient of Variation as a percentage (CV\%). }
  \item{[27]}{ the Lower 95\% Confidence Limit on the Median. }
  \item{[28]}{ the Upper 95\% Confidence Limit on the Median. }
  \item{[29]}{ the log10 transformed data (sample) Mean. }
  \item{[30]}{ the log10 transformed data (sample) Variance. }
  \item{[31]}{ the log10 transformed data (sample) SD. }
  \item{[32]}{ the log10 transformed data (sample) CV\%. }

If the minimum data value is \code{<= 0}, then \code{stats[29:32] <- NA}.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are removed prior to computation.  Depending on the value of \code{iftell}, the \code{NA} count will be displayed, \code{iftell = TRUE}, or suppressed, \code{iftell = FALSE}.

The confidence bounds on the median are estimated via the binomial theorem, not by normal approximation. 
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

