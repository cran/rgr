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
  \item{RepStat}{ the Replicate Status code, see below. }
  \item{xname}{ by default the character string for the data file name, \code{x}, is used for the table title.  An alternate title can be displayed with \code{xname = "text string"}, see Examples. }
  \item{log}{ if a logarithmic transformation of the data is required to meet homogeneity of variance considerations (i.e. severe heteroscedasticity) set \code{log = TRUE}. This is also advisable if the range of the observations exceeds 1.5 orders of magnitude. }
}
\note{
RepStat (Replicate Status) is a code used within the National Geochemical Reconnaissance protocol that indicates an individual geochemical sample's position in the QA/QC structure.  0 indicates a routine survey sample; 1 indicates a routine survey sample which has an associated duplicate sample; and 2 indicates that duplicate sample.

Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data must also be removed prior to running the \code{triples.fgx} function.  This requires care as the data must be in complete duplicate sets.
}
\references{
Garrett, R.G., 2013.  Assessment of local spatial and analytical variability in regional geochemical surveys with a simple sampling scheme.  Geochemistry: Exploration, Environment, Analysis, 13(4):349-354, doi 10.1144.geochem2011-085.
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
