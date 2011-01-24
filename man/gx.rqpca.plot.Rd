\name{gx.rqpca.plot}
\alias{gx.rqpca.plot}
\title{ Function to Plot Principal Component Analysis Loadings and Scores }
\description{
Function to display the results of a Principal Components Analysis (PCA) from the saved object from \code{\link{gx.mva}} or \code{\link{gx.robmva}} as biplots.  Various options for displaying loadings and scores are available, see Details below.
}
\usage{
gx.rqpca.plot(save, v1 = 1, v2 = 2, rplot = TRUE, qplot = TRUE,
	rowids = NULL, ifrot = TRUE, main = "", ...)
}
\arguments{
  \item{save}{ a saved object from the execution of function \code{\link{gx.mva}} or \code{\link{gx.robmva}}. }
  \item{v1}{ the component to be plotted on the x-axis of the biplot, default is the first component, \code{v1 = 1}. }
  \item{v2}{ the component to be plotted on the y-axis of the biplot, default is the second component, \code{v1 = 2}. }
  \item{rplot}{ the default is to plot the variables.  If the variables are not required set \code{rplot = FALSE}.  Note, if an ilr transform has been undertaken the loadings of the (p-1) synthetic variables will be displayed. }
  \item{qplot}{ the default is to plot the observation (individual, case or sample) scores.  If scores are not required set \code{qplot = FALSE}. }
  \item{rowids}{ \sQuote{switch} to determine if the input matrix row numbers are to be displayed instead of default plotting symbols.  The default is for default plotting symbols, i.e. \code{rowids = NULL}, set \code{rowids = TRUE} if the row numbers are to be displayed. }
  \item{ifrot}{ by default the post-Varimax rotation scores are displayed if a rotation has been made, see \code{\link{gx.rotate}}.  If rotated scores are available in the saved object but the unrotated biplot is to be displayed set \code{ifrot = FALSE}. }
  \item{main}{ an alternate plot title to that in the saved object, see Details below. }
  \item{\dots}{ further arguments to be passed to methods concerning the plot.  For example, if some colour other than black is required for the plotting characters, specify \code{col = 2} to obtain red (see \code{\link{display.lty}} for the default colour palette).  If it is required to make the plot title smaller, add \code{cex.main = 0.9} to reduce the font size by 10\%. }
}
\details{
If \code{main} is undefined the name of the matrix object from which the PCA was derived is passed to the function via the saved object.  Using the matrix name is the recommended procedure in the source functions as it helps to track the progression of the data analysis, acting as a record of the data source.  However, at a presentation stage an alternate plot title may be preferred and can be defined in this function, e.g., \code{main = "Plot Title Text"}.  If no plot title is required set \code{main = " "}.

Information on the percentage of the variability explained by each component, and whether or not rotation has been undertaken, is recoved from the saved object and used to appropriately label the plot axes.

The following describes the available plot option combinations, the first being the default:\cr
rplot = TRUE & qplot = TRUE & rowids = NULL, crosses (pch default) and variable names\cr
rplot = TRUE & qplot = FALSE & rowids = NULL, variable names only\cr
rplot = FALSE & qplot = TRUE & rowids = NULL, crosses (pch default) only\cr
rplot = FALSE & qplot = TRUE & rowids = TRUE, input matrix row numbers only\cr
rplot = TRUE & qplot = TRUE & rowids = TRUE, input matrix row numbers and variable names

Because functions \code{\link{gx.mva}} and \code{\link{gx.robmva}} require a matrix as input the sample IDs that may be in a dataframe are lost.  To plot in the component score space with Sample IDs, the scores can be recovered from the saved object, e.g., \code{save$rqscore[, 1]} and \code{save$rqscore[, 2]}, and used as the x- and y-coordinates in function \code{xyplot.tags} with the sample IDs from the source dataframe. Appropriate plot and axis titling can be displayed by setting the function arguments \sQuote{by hand}.
}
\references{
Reimann, C., Filzmoser, P., Garrett, R. and Dutter, R., 2008. Statistical Data Analysis Explained: Applied Environmental Statistics with R. John Wiley & Sons, Ltd., 362 p.

Venables, W.N. and Ripley, B.D., 2001. Modern Applied Statistics with S-Plus, 3rd Edition, Springer,  501 p.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.mva}}, \code{\link{gx.robmva}}, \code{\link{xyplot.tags}} }
\examples{
## Make test data available
data(sind)
attach(sind)
sind.mat <- as.matrix(sind[, -c(1:3)])
## Ensure all data are in the same units (mg/kg)
sind.mat2open <- sind.mat
sind.mat2open[, 2] <- sind.mat2open[, 2] * 10000

## Save PCA results and display biplots
sind.save <- gx.mva(clr(sind.mat2open))
gx.rqpca.plot(sind.save)
gx.rqpca.plot(sind.save,
main = "Howarth & Sinding Larsen Stream Sediments\nclr transform",
pch = 4, cex.main = 0.9)
gx.rqpca.plot(sind.save, rplot = TRUE, qplot = FALSE, rowids = NULL)
gx.rqpca.plot(sind.save, rplot = FALSE, qplot = TRUE, rowids = NULL)
gx.rqpca.plot(sind.save, rplot = FALSE, qplot = TRUE, rowids = TRUE,
cex = 0.9)
gx.rqpca.plot(sind.save, rplot = TRUE , qplot = TRUE, rowids = TRUE,
cex = 0.9)
#
attach(sind)
xyplot.tags(sind.save$rqscore[, 1],sind.save$rqscore[, 2], ID, cex = 0.9)

## Clean-up and detach test data
rm(sind.mat)
rm(sind.mat2open)
rm(sind.save)
detach(sind)
}
\keyword{ hplot }

