\name{gx.rotate}
\alias{gx.rotate}
\title{ Function to Perform a Kaiser Varimax Rotaion }
\description{
Function to perform a Kaiser Varimax rotation on Principal Component (PCA) loadings and scores in an object saved from \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}} or \code{\link{gx.robmva.closed}}.
}
\usage{
gx.rotate(save, nrot = 2)
}
\arguments{
  \item{save}{ a saved object from the execution of function \code{\link{gx.mva}}, \code{\link{gx.robmva}}, or \cr\code{gx.robmva.closed}. }
  \item{nrot}{ the number of component loadings to be rotated, by default the first two components are rotated, \code{nrot = 2}. }
}
\value{
The value of \code{nr} is modified in, and the following are appended to, the object that was saved from \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}}, or \code{\link{gx.robmva.closed}}:
  \item{nr}{ modified to equal the number of components rotated. }
  \item{vload}{ the new loadings after Varimax rotation. }
  \item{vscore}{ the new scores after Varimax rotation. } 
  \item{vvcontrib}{ the contribution of the rotated Varimax component to the total data variability. }
  \item{pvvcontrib}{ the contribution of the rotated Varimax component to the total data variability as a percentage. }
  \item{cpvvcontrib}{ the cumulative contribution of the rotated Varimax component to the total data variability as a percentage. }
}
\references{
Reimann, C., Filzmoser, P., Garrett, R. and Dutter, R., 2008. Statistical Data Analysis Explained: Applied Environmental Statistics with R. John Wiley & Sons, Ltd., 362 p.

Venables, W.N. and Ripley, B.D., 2001. Modern Applied Statistics with S-Plus, 3rd Edition, Springer,  501 p.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}}, \code{\link{varimax}}, \code{\link{gx.rqpca.loadplot}}, \cr\code{\link{gx.rqpca.plot}} }
\examples{
## Make test data available
data(sind.mat2open)

## Save PCA results and display biplots before and after Varimax rotation
sind.save <- gx.mva(clr(sind.mat2open))
gx.rqpca.plot(sind.save)
gx.rqpca.plot(sind.save,
main = "Howarth & Sinding Larsen Stream Sediments\nclr transform",
pch = 4, cex.main = 0.9)
sind.save.rot2 <- gx.rotate(sind.save)
gx.rqpca.plot(sind.save.rot2,
main = "Howarth & Sinding Larsen Stream Sediments\nclr transform",
pch = 4, cex.main = 0.9)

## Clean-up
rm(sind.save)
rm(sind.save.rot2)
}
\keyword{ multivariate }

