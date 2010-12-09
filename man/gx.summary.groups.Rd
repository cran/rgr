\name{gx.summary.groups}
\alias{gx.summary.groups}
\title{ Displays Summary Statistics for a Variable Grouped by a Factor }
\description{ 
Displays the same concise one-line summary statistics report as \code{\link{gx.summary1}} but with the data grouped by the value of a factor variable.  The table consists of a heading line and a line of summary statistics for each \sQuote{group}, value of the factor variable.
}
\usage{
gx.summary.groups(group, x, xname = deparse(substitute(x)), log = FALSE)
}
\arguments{
  \item{group}{ the name of the factor variable the data are to be grouped by. }
  \item{x}{ name of the variable to be processed. }
  \item{xname}{ an informative display title, e.g., \code{xlab = "Cu (mg/kg) in <2 mm O-horizon soil"} rather than the default variable name of \cr\code{deparse(substitute(xx))}. }
  \item{log}{ if the summary statistics are required following a log10 transformation, set \code{log = TRUE}. }
}
\details{
Setting \code{log = TRUE} results in a log transformation for the parametric statistical estimates.  The maximum, minimum, quartiles and robust estimates of spread are estimated and reported in natural measurement units.  Of the parametric statistics, the mean (the geometric mean) and 95\%confidence are reported backtransformed into natural measurement units.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data vector, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector will be removed prior to computation in function \code{gx.stats}.

Alternately, function \code{\link{framework.summary}} generates grouped summary statistics that are exported in a file format that can be directly imported into a spreadsheet, e.g., MS Excel, for inspection, or into other software, e.g., a Geographical Information System (GIS) where the spatial information concerning the \sQuote{framework} units is available, e.g., ecoclassification units.

For more extensive summary statistics displaying one variable at a time, see \code{\link{gx.summary2}} using a construct like \code{gx.summary2(var[factor == "value"])} or use function \code{\link{inset}} with a similar construct.

For summary graphical presentations see functions \code{\link{bwplot}} or \code{\link{tbplot}}.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.summary1}}, \code{\link{gx.summary}}, \code{\link{gx.stats}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{gx.summary2}} }
\examples{
data(kola.c)
attach(kola.c)

## Generates an initial display
gx.summary.groups(COUNTRY, Cu)

## Provide a more informative display
gx.summary.groups(COUNTRY, Cu, "Cu (mg/kg) in <2 mm O-horizon soil")

## As above but with a log10 transformation to display
## the geometric mean, etc.
gx.summary.groups(COUNTRY, Cu, "Cu (mg/kg) in <2 mm O-horizon soil",
	log = TRUE)

## Detach test data
detach(kola.c)
}
\keyword{ univar }

