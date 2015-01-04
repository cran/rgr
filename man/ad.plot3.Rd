\name{ad.plot3}
\alias{ad.plot3}
\title{ Plot Results of Analytical Duplicate Analyses as Ratios }
\description{
Function to plot the results of analytical duplicate analyses as ratios vs. their means with logarithmic scaling.  Classical and robust estimates for the mean/median and Relative Standard Deviations (\%) are displayed.  In addition to displaying this information the 95\% standard error interval on the mean ratio are estimated and used to test that the mean ratio is not significantly different from one.  A situation that would indicate severe systematic drift between the analytical duplicates, something that could happen if the duplicates were analysed in a separate batch at a later date, or were analysed as a group at the beginning or end of an analytical batch.  The 95\% confidence bounds for the duplicate ratios are plotted as dotted lines on the plot.  If the lower detection for the analyses is defined, and it falls within the range of the duplicate meams, it will be plotted as a vertical dotted line.  To facilitate comparison between measurements, or measurements by different methods, the range of the ratio display can be defined.  Optionally, the locator may be deployed so that the ratios for estimating the 95\% confidence bounds on any value may be placed on the plot in a location of the user's choice.  Optionally the ratio plot may be supplemented by an CPP of the ratios, in which case they are presented as side-by-side plots.  For data stored in alternate forms from that expected by this function use \code{\link{ad.plot4}}.
}
\usage{
ad.plot3(x1, x2, xname = deparse(substitute(x1)), if.order = TRUE, ad.tol = NULL,
        ldl = NULL, maxrat = NULL, if.text = FALSE, if.cpp = FALSE, ...)
}
\arguments{
  \item{x1}{ a column vector from a matrix or data frame, \code{x1[1], ..., x1[n]}. }
  \item{x2}{ another column vector, \code{x2[1], ..., x2[n]}.  \code{x1} and \code{x2} must be of identical length, where \code{x2} is a duplicate measurement of \code{x1}. }
  \item{xname}{ a title can be displayed with the plot and results, e.g., \code{xname = "Cu (mg/kg)"}.  If this field is undefined the character string for \code{x} is used as a default. } 
  \item{if.order}{ by default the analytical duplicate results are plotted in the order in which they occur in the data file, this usually corresponds to date of analysis.  Alternately, setting \code{if.order = FALSE} results in the results being plotted against their means. }
  \item{ad.tol}{ a tolerance level may be provided for the maximum acceptable percent ratio between duplicates when they are plotted sequentially, i.e. \code{if.order = TRUE}, in which case red dotted lines are added to the plot. }
  \item{ldl}{ if the lower detection limit (ldl) of the analytical procedure is provided and falls within the range of the duplicate means it will be plotted as a vertical black dotted line.  If another colour is required, e.g., red, set \code{col = 2}. See function \code{\link{display.lty}} for the range of available colours. }
  \item{maxrat}{ optionally the maximum ratio for the plot axis, the minimum is computed as \code{1/maxrat}, may be set.  This is useful when making comparisons between elements or between measurement methods and ensures similar scaling.  Otherwise \code{maxrat} is determined from the data. }
  \item{if.text}{ if set \code{if.text = TRUE} the locator is deployed so that the ratios for estimating the 95\% confidence bounds on any value may be placed on the ratio plot. }
  \item{if.cpp}{ if set \code{if.cpp = TRUE} the ratio plot is displayed side-by-side with an Normal Cumulative Probability Plot (CPP) of the ratios.  In this instance \code{if.text} is not an option. }
  \item{\dots}{ any additional arguments to be passed to the \code{plot} function for titling, etc. }
}
\details{
If the data are as a single concatenated vector from a matrix or data frame as \code{x1[1], ..., x1[n]} followed by \code{x[n+1], ..., x[2n]}, or alternated as \code{x[1]} and \code{x[2]} being a pair through to \code{x[2*i+1]} and \code{x[2*i+2]} for the \code{i in 1:n} duplicate pairs use function \code{\link{ad.plot4}}.

For examples see \code{\link{ad.plot4}} as Geological Survey of Canada National Geochemical Reconnaissance survey data are not stored in this format.  This function is present as the graphical equivalent to \code{\link{anova1}}.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ad.plot4}}, \code{\link{ad.plot2}}, \code{\link{ltdl.fix.df}} }
\keyword{ univar }
\keyword{ htest }
\keyword{ hplot }
