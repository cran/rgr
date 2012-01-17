\name{gx.summary2}
\alias{gx.summary2}
\title{ Display a ten-line Summary Statistics Report }
\description{
Displays a more extensive report than \code{gx.summary1}. The report includes sample size, number of NAs in the input vector; arithmetic mean and 95\% confidence limits, standard deviation and CV\%; geometric mean and 95\% confidence limits, with standard deviation and CV\% in log10 units; median and 95\% confidence limits robust estimates of spread (MAD and interquartile based measure); and minimum, maximum, quartiles, and 2nd, 5th, 10th, 90th, 95th and 98th percentiles.}
\usage{
gx.summary2(xx, xname = deparse(substitute(xx)))
}
\arguments{
  \item{xx}{ name of the variable to be processed. }
  \item{xname}{ an informative display title, e.g., \cr\code{xname = "Cu (mg/kg) in <2 mm O-horizon soil"} rather than the default variable name of \code{xname = deparse(substitute(xx))}. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data vector, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector will be removed prior to computation in function \code{gx.stats}.

For a less extensive summary statistics display, see \code{\link{gx.summary1}}.  For summary graphical displays see \code{\link{shape}} or \code{\link{inset}}.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.summary}}, \code{\link{gx.stats}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{gx.summary1}} }
\examples{
## Make test data available
data(kola.o)
attach(kola.o)

## Generates an initial display
gx.summary2(Cu)

## Provide a more informative display
gx.summary2(Cu, "Cu (mg/kg) in <2 mm Kola O-horizon soil")

## Detach test data
detach(kola.o)
}
\keyword{ univar }

