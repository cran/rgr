\name{ad.plot1}
\alias{ad.plot1}
\title{ Plot Results of Analytical Duplicate Analyses }
\description{
Function to plot the results of analytical duplicate analyses as the percent absolute difference between duplicates relative to their means, or as the Relative Standard Deviation of the individual duplicate.  Classical and robust estimates of the arithmetic Relative Standard Deviation (\%) and the mean/median to which they apply are displayed based on the pooled estimate of analytical variability from the duplicates.  If the duplicate data span more that 1.5 orders of magnitude this estimate is unreliable due to heterogeneity of variance considerations (heteroscedasticity).  The x-axis data may either present the duplicates in the order in which they occur in the data, usally a time-series, as the duplicate means, or as the ratio of the duplicate mean to the lower detection limit if it is provided.  Optionally the x-axis may be scaled logarithmically if the range of the data requires.  If there is a target acceptance level it may be provided and will be displayed as a red dashed line on the plot.  For data stored in alternate forms from that expected by this function use \code{\link{ad.plot2}}.
}
\usage{
ad.plot1(x1, x2, xname = deparse(substitute(x1)), if.order = TRUE,
	if.rsds = FALSE, ldl = NULL, ad.tol = NULL, log = FALSE, ...)
}
\arguments{
  \item{x1}{ a column vector from a matrix or data frame, \code{x1[1], ..., x1[n]}. }
  \item{x2}{ another column vector from the matrix or data frame,  \code{x2[1], ..., x2[n]}. \code{x1} and \code{x2} must be of identical length, \code{n}, where \code{x2} is a duplicate measurement of \code{x1}. }
  \item{xname}{ a title can be displayed with the plot and results, e.g., \code{xname = "Cu (mg/kg)"}.  If this field is undefined the character string for \code{x} is used as a default. }
  \item{if.order}{ by default the analytical duplicate results are plotted in the order in which they occur in the data file, this usually corresponds to date of analysis.  Alternately, setting 
\code{if.order = FALSE} results in the results being plotted against their means. }
  \item{if.rsds}{ by default the absolute difference between the duplicates expressed as a percentage of their mean is plotted on the y-axis.  If it is required to plot the individual duplicate relative standard deviations (RSDs), set \code{if.rsds = TRUE}. }
  \item{ldl}{ by default the x-axis is defined by the measurement units.  If it is desired to express the duplicate means as a ratio to the lower detection limit (ldl) of the analytical procedure, then set \code{ldl = 'ldl'} in measurement units. }
  \item{ad.tol}{ optionally a tolerance level may be provided for the maximum acceptable percent absolute relative difference between duplicates, in which case a red dotted line is added to the plot.  When \code{if.rsds = TRUE} the value of \code{ad.tol} is interpreted to be the maximum acceptable individual duplicate relative standard deviation. }
  \item{log}{ optionally the x-axis of the plot employing duplicate means may be plotted with logarithmic scaling, if so, set \code{log = TRUE}. }
  \item{\dots}{ any additional arguments to be passed to the \code{plot} function for titling, etc. }
}
\details{
If the data are as a single concatenated vector from a matrix or data frame as \code{x1[1], ..., x1[n]} followed by \code{x[n+1], ..., x[2n]}, or alternated as \code{x[1]} and \code{x[2]} being a pair through to \code{x[2*i+1]} and \code{x[2*i+2]} for the \code{i in 1:n} duplicate pairs use function \code{\link{ad.plot2}}.

For examples see \code{\link{ad.plot2}} as Geological Survey of Canada National Geochemical Reconnaissance survey data are not stored in this format.  This function is present as the graphical equivalent to \code{\link{anova1}}.
}
\note{ Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}. }
\author{ Robert G. Garrett }
\seealso{ \code{\link{ad.plot2}}, \code{\link{ad.plot3}}, \code{\link{ltdl.fix.df}} }
\keyword{ univar }
\keyword{ hplot }
