\name{gx.summary}
\alias{gx.summary}
\title{ Compiles a Table of Summary Statistics }
\description{
This function is a \sQuote{sub-engine} between the main summary statistics engine, \sQuote{gx.stats}, and the \code{gx.summary.*} display functions.  Its \sQuote{sub-engine} function is to select the required results from the \code{gx.stats} computations, and additionally compute 95\% confidence bounds on means.
}
\usage{
gx.summary(xx, log = log, iftell = iftell)
}
\arguments{
  \item{xx}{ name of the variable to be processed. }
  \item{log}{ if it will be required to display summary statistics following a log10 transformation of the data, set \code{log = TRUE}. }
  \item{iftell}{ passes the value of \code{iftell} for controlling the display of the \code{NA} count to function \code{remove.na} from the calling function. }
}
\value{
  \item{table}{ a 15-element vector containing summary statistics, see below: }
  \item{[1]}{ the sample size, N. }
  \item{[2]}{ the number of NAs removed from the data passed for processing. }
  \item{[3:7]}{ the minimum value, Q1, Median, Q3 and maximum value. }
  \item{[8]}{ the Median Absolute Deviation (MAD). }
  \item{[9]}{ the Inter-Quartile Standard Deviation (IQSD). }
The contents of elements \code{[10:15]} depend on the \sQuote{value} of \code{log} 
  \item{[10]}{ the data (sample) Mean. } 
  \item{[11]}{ the data (sample) Standard Deviation (SD). }
  \item{[12]}{ the Coefficient of Variation as a percentage (CV\%). }
  \item{[13]}{ the Standard Error (S.E.) of the Mean. }
  \item{[14]}{ the Lower 95\% Confidence Limit on the Mean. }
  \item{[15]}{ the Upper 95\% Confidence Limit on the Mean. }
If \code{log = TRUE}, the results for the mean, \code{[13]}, and confidence limits, \code{[14:15]}, are backtransformed to the natural scale.

The returned table is rounded to 4 significant figures.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing \code{gx.summary.*} functions that call this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector will be removed prior to computation in function \code{gx.stats}.  Depending on the value of \code{iftell}, the \code{NA} count will be displayed, \code{iftell = TRUE}, or suppressed, \code{iftell = FALSE}.

There are no examples for this function.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.stats}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, }
\keyword{ univar }
 
