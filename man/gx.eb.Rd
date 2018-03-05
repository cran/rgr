\name{gx.eb}
\alias{gx.eb}
\title{ Computation of Empirical Balances }
\description{
Computes empirical balances (ratios) for the stated columns of a \code{n} by \code{p} matrix of compositional data.
}
\usage{
gx.eb(r, s, xx, ...)
}
\arguments{
  \item{r}{ number of parts in the numerator. }
  \item{s}{ number of parts in the denominator. }
  \item{xx}{ matrix for which the balances for the stated columns will be computed. }
  \item{...}{ the column indices of the parts in the numerator, followed by the column indices for the parts in the denominator.  The total number of indices must equal the sum of \code{r} and \code{s}. }
}
\value{
  \item{z}{ the vector of \code{n} balances. }
}
\note{
Multi-element ratios have a long history in exploration geochemistry, the parts in numerator and denominator being selected on the basis of prior knowledge of the mineralogy and geochemistry of the feature, commonly a mineral occurrence, being sought.  As the features are rare events ratios can be used to accentuate their numerical expression to increase their \sQuote{contrast} from the main mass of background data.  The use of balances, ratios of geometric means of the parts in the numerator and denominator, accomodates the compositional nature of geochemical data.
}
\references{
Egozcue, J.J. & Pawlowsky-Glahn, V., 2005. Groups of Parts and Their Balances in Compositional Data Analysis. Mathematical Geology, 37(7):795-828.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}  }
\examples{
## Make test data available
data(sind.mat2open)

## Compute and display empirical balances for columns 1, 5 & 4 
## of the data vs. columns 2 & 3
temp <- gx.eb(3, 2, sind.mat2open, 1, 5, 4, 2, 3)
shape(temp, "Zn.Cu.Cd/Fe.Mn balance")

## Clean-up
rm(sind.mat2open)
rm(temp)
}
\keyword{ multivariate }

