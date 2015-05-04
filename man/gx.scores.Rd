\name{gx.scores}
\alias{gx.scores}
\title{ Function to Compute Scores on the Basis of Threshold Estimates }
\description{
Computes scores for a user selected group of variables based on the ratio of variable value to the variable threshold, i.e. the upper limit of background variation.  The user must provide thresholds for the variables contributing to the scores.  Optionally a set of relative weights may be provided that are applied to the scores.  If above threshold values occur for a variable whose influence is indicative of a \sQuote{false} anomaly the relative weight for that variable may be set \sQuote{-ve}, which will result in a reduction of the computed score.  This function is a useful alternative to weighted sums when the variable data contains so many below DL values that summary statistics cannot be estimated. An object is created containing all the provided parameters and the scores for later reference and use.
}
\usage{
gx.scores(xx, tholds, rwts = NULL, setna = FALSE)
}
\arguments{
  \item{xx}{ name of the \code{n} by \code{p} matrix containing the data. }
  \item{tholds}{ a vector of the threshold estimates for the \code{p} variables. }
  \item{rwts}{ an optional vector of the relative weights for the \code{p} variables, negative weights are permisable to indicate that high levels of the variable should have a negative impact on the scores. }
  \item{setna}{ if it is required to set any \sQuote{<=0} scores to NAs then set \code{setna = TRUE}.  See note below.  }
}
\details{
If the data for only some of the variables available in an attached matrix or data frame are to be processed use the \code{cbind} construct.  Thus, \code{temp.mat <- cbind(vname1, vname3, vname6, vname8)}, or the cbind may be used directly, see Example below.  All computed scores with values less that 1 are set to zero, optionally these may be replaced by NAs to facilitate their removal from subsequent plots, see Note below.
}
\value{
The following are returned as an object to be saved for further use:
   \item{input}{ the name of the input data set }
   \item{tholds}{ the vector of thresholds used for the computations }
   \item{rwts}{ the vector of relative weights provided by the user }
   \item{scores}{ the computed scores }
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any rows in the data matrix with with \code{NA}s are removed prior to computing the weighted sums.

In general, it is not advisable to replace zeros with NAs as the overall data or spatial structure of the data will not be discerned in subsequent plots as records with NAs will have been removed.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{remove.na}} }
\examples{
##  Make test data available
data(sind)
attach(sind)

##  Compute scores - 1
sind.scores1 <- gx.scores(cbind(Cu, Zn, Cd), tholds = c(100, 200, 2))
map.z(E, N, sind.scores1$scores)

##  Compute scores - 2
sind.scores2 <- gx.scores(cbind(Cu, Zn, Cd, Fe), tholds = c(100, 200, 2, 2),
	rwts = c(1, 1, 1, -1))
xyplot.z(Fe, Mn, sind.scores2$scores, col = 2)

## Clean up and detach test data
rm(sind.scores1)
rm(sind.scores2)
detach(sind)
}
\keyword{ multivariate }

