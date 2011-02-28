\name{gx.summary1}
\alias{gx.summary1}
\title{ Display a one-line Summary Statistics Report }
\description{
Displays a concise one-line summary statistics report, below a heading line, consisting of sample size, number of NAs in the input vector; minimum, maximum and quartiles; robust estimates of the standard deviation (MAD and interquartile based measure); mean, standard deviation and coefficient of variation (\%); and the standard error, and lower and upper 95\% confidence limits on the mean.  See Details for the results of setting \code{log = TRUE}.
}
\usage{
gx.summary1(xx, xname = deparse(substitute(xx)), log = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xx}{ name of the variable to be processed. }
  \item{xname}{ an informative display title, e.g., \code{xlab = "Cu (mg/kg) in <2 mm O-horizon soil"} rather than the default variable name of \cr\code{deparse(substitute(xx))}. }
  \item{log}{ if the summary statistics are required following a log10 transformation, set \code{log = TRUE}. }
}
\details{
Setting \code{log = TRUE} results in a log transformation for the parametric statistical estimates.  The maximum, minimum, quartiles and robust estimates of spread are estimated and reported in natural measurement units.  Of the parametric statistics, the mean (the geometric mean) and 95\%confidence are reported backtransformed into natural measurement units.  If all the results are required following a log10, or some other transformation, this can be achieved by executing the transformation in thecall, e.g., \code{gx.summary1(log10(Cu))} or \code{gx.summary1(sqrt(Cu))}.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data vector, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector will be removed prior to computation in function \code{gx.stats}.

For a more extensive summary statistics display, see \code{\link{gx.summary2}}.  For summary graphical displays see \code{\link{shape}} or \code{\link{inset}}.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.summary}}, \code{\link{gx.stats}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{gx.summary2}} }
\examples{
## Make test data available
data(kola.o)
attach(kola.o)

## Generates an initial display
gx.summary1(Cu)

## Provide a more informative display
gx.summary1(Cu, "Cu (mg/kg) in <2 mm Kola O-horizon soil")

## As above but with a log10 transformation to display
## the geometric mean, etc.
gx.summary1(Cu, "Cu (mg/kg) in <2 mm Kola O-horizon soil", log = TRUE)

## Detach test data
detach(kola.o)
}
\keyword{ univar }
