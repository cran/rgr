\name{ad.plot4}
\alias{ad.plot4}
\title{ Plot Results of Analytical Duplicate Analyses as Ratios, Alternate Input }
\description{
Function to prepare data stored in alternate forms from that expected by function \code{\link{ad.plot3}} for its use, for further details see \code{x} in Arguments below.  The data will be plotted as ratios, i.e. \code{dup1/dup2}, with logarithmic scaling relative to their means.
}
\usage{
ad.plot4(x, xname = deparse(substitute(x)), if.order = FALSE, ad.tol = NULL,
        ifalt = FALSE, ldl = NULL, maxrat = NULL, if.text = FALSE,
        if.cpp = FALSE, ...)
}
\arguments{
  \item{x}{ a column vector from a matrix or data frame, \code{x[1], ..., x[2*n]}. The default is that the first n members of the vector are the first measurements and the second n members are the duplicate measurements.  If the measurements alternate, i.e. duplicate pair 1 measurement 1 followed by measurement 2, etc., set \code{ifalt = TRUE}. }
  \item{xname}{ a title can be displayed with the plot and results, e.g., \code{xname = "Cu (mg/kg)"}.  If this field is undefined the character string for \code{x} is used as a default. }
  \item{if.order}{ by default the analytical duplicate results are plotted in the order in which they occur in the data file, this usually corresponds to date of analysis.  Alternately, setting \code{if.order = FALSE} results in the results being plotted against their means. }
  \item{ad.tol}{ optionally a tolerance level may be provided for the maximum acceptable percent ratio between duplicates, in which case a red dotted line is added to the plot. }
  \item{ifalt}{ set \code{ifalt = TRUE} to accommodate alternating sets of paired observations. }
  \item{ldl}{ if the lower detection limit (ldl) of the analytical procedure is provided and falls within the range of the duplicate means it will be plotted as a vertical black dotted line.  If another colour is required, e.g., red, set \code{col = 2}. See function \code{\link{display.lty}} for the range of available colours. }
  \item{maxrat}{ optionally the maximum ratio for the plot axis, the minimum is computed as \code{1/maxrat}, may be set.  This is useful when making comparisons between elements or between measurement methods and ensures similar scaling.  Otherwise \code{maxrat} is determined from the data. }
  \item{if.text}{ if set \code{if.text = TRUE} the locator is deployed so that the ratios for estimating the 95\% confidence bounds on any value may be placed on the ratio plot. }
  \item{if.cpp}{ if set \code{if.cpp = TRUE} the ratio plot is displayed side-by-side with an Normal Cumulative Probability Plot (CPP) of the ratios.  In this instance \code{if.text} is not an option. }
  \item{\dots}{ any additional arguments to be passed to the \code{plot} function for titling, etc. }
}
\details{
Data may be as a single concatenated vector from a matrix or data frame as \code{x1[1], ..., x1[n]} followed by \code{x[n+1], ..., x[2n]}, or alternated as \code{x[1]} and \code{x[2]} being a pair through to \code{x[2*i+1]} and \code{x[2*i+2]} for the \code{i in 1:n} duplicate pairs, see \code{ifalt}.

If the data are as n duplicate pairs, \code{x1} and \code{x2}, use function \code{\link{ad.plot3}}.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ad.plot3}}, \code{\link{ad.plot1}}, \code{\link{ltdl.fix.df}} }
\examples{
## Make test data available
data(ad.test)
attach(ad.test)

## Plot analytical duplicate analyses as ratios in acquistion sequence
ad.plot4(Cu, ifalt = TRUE)

## Plot analytical duplicate analysis ratios against their means with
## more informative annotation
ad.plot4(Cu, "Cu (mg/kg)", ifalt = TRUE, if.order = FALSE)

## Plot analytical duplicate analysis ratios against their means with a
## CPP of the ratios and more informative annotation
ad.plot4(Cu, "Cu (mg/kg)", ifalt = TRUE, if.order = FALSE, if.cpp = TRUE)

## Detach test data
detach(ad.test)
}
\keyword{ univar }
\keyword{ htest }
\keyword{ hplot }
