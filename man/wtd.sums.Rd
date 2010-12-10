\name{wtd.sums}
\alias{wtd.sums}
\title{ Function to Compute Weighted Sums }
\description{
Computes weighted sums for a user selected group of variables (Garrett and Grunsky, 2001).  The user must provide the relative importances of the the variables contributing to the weighted sums.  By default the median and MAD are estimated as measures of location and spread for the data.  These may be replaced with alternate estimates if the user wishes, see Details below.  An object is created containing all the estimated parameters and the weighted sums for later reference and use.
}
\usage{
wtd.sums(xx, ri, xloc = NULL, xspread = NULL)
}
\arguments{
  \item{xx}{ name of the n by p matrix containing the data. }
  \item{ri}{ a vector of the relative weights for the p variables, negative weights are permisable to indicate that high levels of the variable should have a negative impact on the weighted sums. }
  \item{xloc}{ the default procedure is to use the computed medians of the input variables.  Alternately, a vector of p estimates of location may be provided. }
  \item{xspread}{ the default procedure is to use the computed MADs of the input variables.  Alternately, a vector of p estimates of spread may be provided. }
}
\details{
If the data for only some of the variables available in an attached matrix or dataframe are to be processed use the \code{cbind} construct.  Thus, \code{temp.mat <- cbind(varname1, varname3, varname6, varname8)}, or the cbind may be used directly, see Example below. 
}
\value{
The following are returned as an object to be saved for further use:
   \item{input}{ the name of the input data set }
   \item{xloc}{ the vector of locations used for the computations }
   \item{xspread}{ the vector of spreads used for the computations }
   \item{ri}{ the vector of relative importances provided by the user }
   \item{w}{ the vector of weights computed from the relative importances }
   \item{a}{ the vector of coefficients - the normalized weights }
   \item{[ws]}{ the computed weighted sums }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data vector, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any rows in the data matrix with with \code{NA}s are removed prior to computing the weighted sums.
}
\author{ Robert G. Garrett }
\references{
Garrett, R.G. and Grunsky, E.G., 2001. Weighted Sums - Knowledge based empirical indices for use in exploration geochemistry.  Geochemistry: Exploration, Environment and Analysis, 1(2):135-141.
}
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}} }
\examples{
##  Make test data available
data(sind)
attach(sind)

##  Compute weighted sums as in Garrett & Grunsky (2001)
##  using medians and interquartile SDs
sind.ws.geea <- wtd.sums(cbind(Zn, Cd, Fe, Mn), ri = c(2, 1, -1, -1),
	xloc = c(48, 0.6, 1.74, 590), 
	xspread = c(41.5128, 0.44478, 0.882147, 333.585))

## Compute weighted sums using the median and MAD defaults
sind.ws.def <- wtd.sums(cbind(Zn, Cd, Fe, Mn), ri = c(2, 1, -1, -1))

## Plot the two results against one-another, adding a constant,
## 3, to the weighted sums to make them positive and log-scale
## plottable
par(pty="s")
plot(sind.ws.geea$ws+3, sind.ws.def$ws+3, log = "xy", 
	xlim = c(2, 28), ylim = c(2, 28))
abline(0, 1, lty = 3)
abline(v =3, lty = 3)
abline(h = 3, lty = 3)

## Inspect the default weighted sums, adding a constant, 3, to the
## weighted sums to make them positive and log-scale plottable
shape(sind.ws.def$ws+3, log = TRUE)

## Plot EDA Tukey boxplot based map of default weighted sums
map.eda7(E, N, sind.ws.def$ws)

## Clean-up and detach test data
rm(sind.ws.geea)
rm(sind.ws.def)
par(pty = "m")
detach(sind)
}
\keyword{ multivariate }
