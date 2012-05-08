\name{gx.rqpca.loadplot}
\alias{gx.rqpca.loadplot}
\title{ Function to Graphically Display PCA Loadings }
\description{
Function to graphically display PCA loadings computed by functions \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}} or \code{\link{gx.rotate}}.  The user may define the minimum absolute loading below which variables will not be graphically displayed, and modify the display title and text size as required. 
}
\usage{
gx.rqpca.loadplot(save, main = "", crit = 0.3, cex = 0.8, 
	cex.axis = 0.7, cex.main = 0.8)
}
\arguments{
  \item{save}{ a saved object from any of functions \code{gx.mva}, \code{gx.robmva} or \cr\code{gx.robmva.closed}. }
  \item{main}{ an alternate plot title from that generated automatically from information in the saved object, see Details below. }
  \item{crit}{ the lower limit of the absolute value of a loading for a variable to be displayed, by default \code{crit = 0.3}. }
  \item{cex}{ the text scale expansion factor for the variable names in the display, by default \code{cex = 0.8}, a 20\% font size reduction. }
  \item{cex.axis}{ the text scale expansion factor for the axis labels of the display, by default \code{cex.axis = 0.7}, a 30\% font size reduction. }
  \item{cex.main}{ the text scale expansion factor for the display title, by default \code{cex.axis = 0.8}, a 20\% font size reduction. }
}
\details{
If \code{main} is undefined the name of the matrix object supplied to the function is displayed, together with the value of \code{crit}.  On the line below the name of the data matrix from which the PCA was derived is displayed.  However, if an alternate plot title is preferred it may be defined, e.g., \code{main = "Plot Title Text"}.  If no plot title is required set \code{main = " "}.

If the variable names are longer than three characters the display can easily become cluttered.  In which case the user should redefine the variable names in the input matrix from which the PCA was derived using the \code{dimnames(matrix.name)[[2]]} construct, and run the generating function again.  Alternately, the variable names in the saved object may be changed directly via a redefinition of \code{save$matnames[[2]]}.
}
\references{
Reimann, C., Filzmoser, P., Garrett, R. and Dutter, R., 2008. Statistical Data Analysis Explained: Applied Environmental Statistics with R. John Wiley & Sons, Ltd., 362 p.
}
\author{ Robert G. Garrett, based on a script by Peter Filzmoser }
\seealso{ \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}}, \code{\link{gx.rotate}} } 
\examples{
## Make test data available
data(sind.mat2open)

## Estimate and display robust PCA loadings
sind.save <- gx.robmva.closed(sind.mat2open)
gx.rqpca.loadplot(sind.save)

## Clean-up
rm(sind.save)
}
\keyword{ hplot }

