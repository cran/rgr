\name{gx.summary.mat}
\alias{gx.summary.mat}
\title{ Displays Summary Statistics for a Matrix or Data Frame }
\description{
Displays the same concise one-line summary statistics report as \code{\link{gx.summary1}} for two or more columns of a matrix or data frame.  The table consists of a heading line and a line of summary statistics for each \sQuote{variable}, column of the matrix or data frame.  Optionally the data may be logarithmically (base 10) transformed.
}
\usage{
gx.summary.mat(xmat, vars, banner = deparse(substitute(xmat)),
	log = FALSE)
}
\arguments{
  \item{xmat}{ name of the matrix or data frame. }
  \item{vars}{ the indices, or names (see Example), of the columns of the matrix or data frame for the variables whose summary statistics are to be displayed. }
  \item{banner}{ an informative display title, e.g., \code{banner = "Kola Project, <2 mm O-horizon soils"} rather than the default variable name of \cr\code{banner = deparse(substitute(xmat))}. }
  \item{log}{if the summary statistics are required following a log10 transformation, set \code{log = TRUE}. }
}
\details{
Setting \code{log = TRUE} results in a log transformation for the parametric statistical estimates.  The maximum, minimum, quartiles and robust estimates of spread are estimated and reported in natural measurement units.  Of the parametric statistics, the mean (the geometric mean) and 95\%confidence are reported backtransformed into natural measurement units.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector will be removed prior to computation in function \code{gx.stats}.  Display of the number of \code{NAs} found by function \code{remove.na} is suppressed in \code{remove.na} as the information is included in the display from this function.

For a more extensive summary statistics display a variable at a time, see \code{\link{gx.summary2}}, and for a summary with graphical displays see \code{\link{inset}}.

For summary graphical presentations see functions \code{\link{bwplots.by.var}} or \code{\link{tbplots.by.var}}.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.summary1}}, \code{\link{gx.summary}}, \code{\link{gx.stats}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{gx.summary2}} }
\examples{
## Make test data available
data(kola.o)
attach(kola.o)

## Generates an initial display for As [6], Co [13], Cu [15],
## Ni [24] and Zn [38] 
gx.summary.mat(kola.o, c(6, 13, 15, 24, 38))

## Alternately
gx.summary.mat(kola.o, c("As", "Co", "Cu", "Ni", "Zn"))

## Provide a more informative display for Be [9], La [19], P [25],
## Th [33], U [35] and Y[37]
gx.summary.mat(kola.o, c(9, 19, 25, 33, 35, 37), 
	banner = "Kola Project, <2 mm O-horizon soils")

## As above but with a log10 transformation to display
## the geometric mean, etc.
gx.summary.mat(kola.o, c("Be", "La", "P", "Th", "U", "Y"), 
	log = TRUE, banner = "Kola Project, <2 mm O-horizon soils")

## Detach test data
detach(kola.o)
}
\keyword{ univar }

