\name{anova2}
\alias{anova2}
\title{ Analysis of Variance (ANOVA), Alternate Input }
\description{
Function to prepare data stored in alternate forms from that expected by function \code{\link{anova1}} for its use. For further details see 'x' in Arguments below . 
}
\usage{
anova2(x, name = deparse(substitute(x)), log = FALSE, ifalt = FALSE)
}
\arguments{
  \item{x}{ a column vector from a matrix or dataframe, \code{x[1], ..., x[2*n]}. The default is that the first n members of the vector are the first measurements and the second n members are the duplicate measurements.  If the measurements alternate, i.e. duplicate pair 1 measurement 1 followed by measurement 2, etc., set \code{ifalt = TRUE}. }
  \item{name}{ a title can be displayed with the results, e.g.,\cr\code{name = "Duplicate measurements of Magnetic Susceptibility"}.  If this field is undefined the character string for \code{x} is used as a default.  }
  \item{log}{ if a logarithmic transformation of the data is required to meet homogeneity of variance considerations (i.e. severe heteroscedasticity) set \code{log = TRUE}. This is also advisable if the range of the observations exceeds 1.5 orders of magnitude. }
  \item{ifalt}{ set \code{ifalt = TRUE} to accommodate alternating sets of paired observations. } 
}
\details{
For further details see \code{\link{anova1}}.

If the data are as n duplicate pairs, x1 and x2, use function \code{\link{anova1}}.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data vector, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{anova1}}, \code{\link{ltdl.fix.df}} }
\examples{
## Make test data ms.data2 available
data(ms.data2)
attach(ms.data2)

## Undertake an ANOVA for duplicate measurements on rock samples
anova2(MS, log = TRUE, 
	name = "Duplicate measurements of Magnetic Susceptibility")

## Detach test data ms.data2
detach(ms.data2)

## Make test data ms.data3 available
data(ms.data3)
attach(ms.data3)

## Undertake an ANOVA for duplicate measurements on rock samples
anova2(MS, log = TRUE, ifalt = TRUE, 
	name = "Duplicate measurements of Magnetic Susceptibility")

## Detach test data ms.data3
detach(ms.data3)
}
\keyword{ univar }
\keyword{ htest }

