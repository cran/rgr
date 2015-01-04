\name{logit}
\alias{logit}
\title{ Logit transformation }
\description{
Undertakes a logit transformation on a vector of proportions. }
\usage{
logit(pp)
}
\arguments{
  \item{pp}{ a vector of proportions in the range zero to one.  The function may be used with a single proportion.  Natural logarithms are used. }
}
\details{
Most analytical chemical data for major, minor and trace elements are of a closed form, i.e. for a sample they sum to a constant, whether it be percent, ppm (mg/kg), or some other units.  It does not matter that only some components contributing to the constant sum are present in the matrix, the data are closed.  As a result, as some elements increase in concentration others must decrease, this leads to statistics and graphical presentations that do not reflect the true underlying situation even in situations of univariate data analysis and display.  The logit transformation is an appropriate transformation for univariate compositional data.  However, for concentrations below 10\% a logarithmic transform is sufficient.  The inverse logit transform is the \code{\link{expit}}.  Procedures for removing closure effects for multivariate data are additive log-ratios (\code{\link{alr}}), centred log-ratios (\code{\link{clr}}), and isometric log-ratios (\code{\link{ilr}}).
}
\value{
  \item{z}{ a vector of the logit tranformations of the proportions \code{p}. }
}
\note{
If a value outside the range zero to one is encountered as a proportion the function displays an error message and halts.

Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data matrix, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

If any \code{NA}s exist in the vector, \code{pp}, they are removed by function \code{\link{remove.na}} and the number removed is displayed.
}
\references{
Filzmoser, P., Hron, K. and Reimann, C., 2009. Univariate statistical analysis of environmental (compositional) data: Problems and possibilities. Science of the Total Environment, 407(1/3):6100-6108.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{expit}}, \code{\link{alr}}, \code{\link{clr}}, \code{\link{ilr}}, \code{\link{ltdl.fix.df}} }
\examples{
## Generate test data
p <- c(0.1, 0.5, 0.9)

## Undertake and display logit transformations
z <- logit(p)
z

## Clean-up
rm(p)
rm(z)
}
\keyword{ univariate }
\keyword{ manip }

