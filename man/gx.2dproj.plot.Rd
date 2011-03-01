\name{gx.2dproj.plot}
\alias{gx.2dproj.plot}
\title{ Function to Display a Saved 2-D Projection Object }
\description{ 
Displays the 2-d projection saved from \code{\link{gx.2dproj}}, optionally the row numbers of the input matrix may be displayed instead of the default plotting symbol.
}
\usage{
gx.2dproj.plot(save, idplot = FALSE, main = "", ...)
}
\arguments{
  \item{save}{ the saved object from \code{\link{gx.2dproj}}. }
  \item{idplot}{ to display the input matrix row numbers set \code{idplot = TRUE}. }
  \item{main}{ an alternative plot title to that in the saved object from \code{\link{gx.2dproj}}, see Details below. }
  \item{\dots}{ further arguments to be passed to methods concerning the plot.  For example, if smaller plotting characters are required, specify \code{cex = 0.8}; or if some colour other than black is required for the plotting characters, specify \code{col = 2} to obtain red (see \code{\link{display.lty}} for the default colour palette).  If it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%. }
}
\details{
If \code{main} is undefined the plot title from the saved object from \code{\link{gx.2dproj}} is displayed.  If no plot title is required set \code{main = " "}, or if a user defined plot title is required it should be defined in \code{main}, e.g., \code{main = "Plot Title Text"}.

The x- and y-axis labels are those in the saved object from \code{\link{gx.2dproj}} and indicate the type of 2-d projection in the display.
}
\references{
Reimann, C., Filzmoser, P., Garrett, R. and Dutter, R., 2008. Statistical Data Analysis Explained: Applied Environmental Statistics with R. John Wiley & Sons, Ltd., 362 p.

Venables, W.N. and Ripley, B.D., 2001. Modern Applied Statistics with S-Plus, 3rd Edition, Springer,  501 p.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.2dproj}} }
\examples{
## Make test data available
data(sind)
sind.mat <- as.matrix(sind[, -c(1:3)])
## Ensure all data are in the same units (mg/kg)
sind.mat2open <- sind.mat
sind.mat2open[, 2] <- sind.mat2open[, 2] * 10000

## Display default 2-D projection
sind.save <- gx.2dproj(ilr(sind.mat2open))

## Display saved object
gx.2dproj.plot(sind.save,
main = "Howarth & Sinding-Larsen\nStream Sediments, ilr Transformed Data")

## Clean-up
rm(sind.mat)
rm(sind.save)
}
\keyword{ hplot }
