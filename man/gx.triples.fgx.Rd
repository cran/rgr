\name{gx.triples.fgx}
\alias{gx.triples.fgx}
\title{ ANOVA to Estimate if \sQuote{Triples} are a Valid Subset }
\description{
Function to execute a simple ANOVA to determine if the Field Duplicates are a valid subset of the regional coverage samples, and if the Field Duplicates pairs have \sQuote{equivalent} variability.  Optionally the data may be logarithmically (base 10) transformed. }
\usage{
gx.triples.fgx(x, RepStat, xname = deparse(substitute(x)),
	log = FALSE)
}
\arguments{
  \item{x}{ a file of regional coverage and field duplicate data. }
  \item{RepStat}{ the Replicate Status code. }
  \item{xname}{ a informative description of the variable in the data file \code{x} for display as a table heading, for example, \cr\code{xname = "Ba (mg/kg - Aqua Regia digestion) in <2 mm unmilled C-horizon soil"}.  The default is simply the data file name, \code{xname = deparse(substitute(x))}. }
  \item{log}{ if a logarithmic transformation of the data is required to meet homogeneity of variance considerations (i.e. severe heteroscedasticity) set \code{log = TRUE}. This is also advisable if the range of the observations exceeds 1.5 orders of magnitude. }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data must also be removed prior to running the \code{triples.fgx} function.  This requires care as the data must be in complete duplicate sets.
}
\references{
Garrett, R.G., submitted.  Assessment of local spatial and analytical variability in regional geochemical surveys with a simple sampling scheme.  Geochemistry: Exploration, Environment, Analysis.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{triples.test1}}, \code{\link{gx.triples.fgx}}, \code{\link{triples.test2}} }
\examples{
## Make test data available
data(triples.test2)
attach(triples.test2)

## Carry out ANOVAs for equivalence of variances
gx.triples.fgx(Ba_ppm, RS, xname = 
"Ba (mg/kg - Aqua Regia digestion) in <2 mm unmilled C-horizon soil")

## Detach test data
detach(triples.test2)
}
\keyword{ univar }
\keyword{ htest }
