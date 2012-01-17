\name{gx.rqpca.screeplot}
\alias{gx.rqpca.screeplot}
\title{ Display a Scree Plot }
\description{ Function to display a scree plot arising from a Principal Components Analysis (PCA) from the saved object from \code{\link{gx.mva}}, \code{\link{gx.robmva}}, or \code{\link{gx.robmva.closed}}.  In addition to the screeplot the cumulative variability explained is also displayed.
}
\usage{
gx.rqpca.screeplot(save, main = "", ...)
}
\arguments{
  \item{save}{ a saved object from the execution of function \code{\link{gx.mva}}, \code{\link{gx.robmva}}, or \cr\code{\link{gx.robmva.closed}}. }
  \item{main}{ an alternate plot title to that in the saved object, see Details below. }
  \item{\dots}{ further arguments to be passed to methods concerning the plot.  For example, if some colour other than black is required for the plotting characters, specify \code{col = 2} to obtain red (see \code{\link{display.lty}} for the default colour palette).  If it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%. }
}
\details{
If \code{main} is undefined the name of the matrix object from which the PCA was derived is passed to the function via the saved object.  Using the matrix name is the recommended procedure in the source functions as it helps to track the progression of the data analysis, acting as a record of the data source.  However, at a presentation stage an alternate plot title may be preferred and can be defined in this function, e.g., \code{main = "Plot Title Text"}.  If no plot title is required set \code{main = " "}.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.mva}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}} }
\examples{
## Make test data available
data(sind.mat2open)

## Save PCA results and display scree plot
sind.save <- gx.mva(ilr(sind.mat2open))
gx.rqpca.screeplot(sind.save)
gx.rqpca.screeplot(sind.save,
main = "Howarth & Sinding Larsen Stream Sediments\nilr transform",
pch = 4, col = 2, cex.main = 0.9)

## Clean-up
rm(sind.save)
}
\keyword{ hplot }

