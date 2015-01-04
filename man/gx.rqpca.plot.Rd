\name{gx.rqpca.plot}
\alias{gx.rqpca.plot}
\title{ Function to Plot Principal Component Analysis Loadings and Scores }
\description{
Function to display the results of a Principal Components Analysis (PCA) from the saved object from \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}} or \code{\link{gx.rotate}} as bi-plots.  Various options for displaying loadings and scores are available, see Details below.
}
\usage{
gx.rqpca.plot(save, v1 = 1, v2 = 2, rplot = TRUE, qplot = TRUE,
	rowids = NULL, ifrot = TRUE, main = "", cex.lab = 0.9,
	cex.main = 0.9, rcex = 1, qcex = 0.8, rcol = 1, qcol = 1, ...)
}
\arguments{
  \item{save}{ a saved object from the execution of function \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}}, \cr\code{\link{gx.robmva.closed}} or \code{\link{gx.rotate}}. }
  \item{v1}{ the component to be plotted on the x-axis of the bi-plot, default is the first component, \code{v1 = 1}. }
  \item{v2}{ the component to be plotted on the y-axis of the bi-plot, default is the second component, \code{v1 = 2}. }
  \item{rplot}{ the default is to plot the variables.  If the variables are not required set \code{rplot = FALSE}.  Note, if an ilr transform has been undertaken the loadings of the (p-1) synthetic variables will be displayed. }
  \item{qplot}{ the default is to plot the observation (individual, case or sample) scores.  If scores are not required set \code{qplot = FALSE}. }
  \item{rowids}{ \sQuote{switch} to determine if the input matrix row numbers are to be displayed instead of default plotting symbols.  The default is for default plotting symbols, i.e. \code{rowids = NULL}, set \code{rowids = TRUE} if the row numbers are to be displayed, or set \code{rowids = FALSE} for the sample IDs to be displayed if they are present in the input matrix, if they are not, row numbers will be displayed. }
  \item{ifrot}{ by default the post-Varimax rotation scores are displayed if a rotation has been made, see \code{\link{gx.rotate}}.  If rotated scores are available in the saved object but the unrotated bi-plot is to be displayed set \code{ifrot = FALSE}. }
  \item{main}{ an alternate plot title from that generated automatically from information in the saved object, see Details below. }
  \item{cex.lab}{ the text scale expansion factor for the axis labels of the display, by default \code{cex.axis = 0.9}, a 10\% font size reduction. }
  \item{cex.main}{ the text scale expansion factor for the display title, by default \code{cex.axis = 0.9}, a 10\% font size reduction. }
  \item{rcex}{ the text scale expansion factor for the variable names in the display, by default \code{cex = 1}, 100\%, no font size reduction. }
  \item{qcex}{ the text scale expansion factor for the observation symbols, row numbers or sample IDs in the display, by default \code{cex = 0.8}, a 20\% font size reduction. }
  \item{rcol}{ the colour of the variable names in the display, by default \code{rcol = 1}, black, setting \code{rcol = 2} will result in red variable names (see \code{\link{display.lty}} for the default colour palette). }
  \item{qcol}{ the colour of the observation symbols, row numbers or sample IDs in the display, by default \code{rcol = 1}, black, setting \code{qcol = 4} will result in blue variable names (see \code{\link{display.lty}} for the default colour palette). }
  \item{\dots}{ further arguments to be passed to methods concerning the plot. }
}
\details{
If \code{main} is undefined the name of the matrix object supplied to the function is displayed in the plot title.  On the line below the name of the data matrix from which the PCA was derived is displayed.  However, if an alternate plot title is preferred it may be defined, e.g., \code{main = "Plot Title Text"}.  If no plot title is required set \code{main = " "}.

If the variable names are longer than three characters the display can easily become cluttered.  In which case the user should redefine the variable names in the input matrix from which the PCA was derived using the \code{dimnames(matrix.name)[[2]]} construct, and run the generating function again.  Alternately, the variable names in the saved object may be changed directly via a redefinition of \code{save$matnames[[2]]}.

Information on the percentage of the variability explained by each component, and whether or not rotation has been undertaken, is recovered from the saved object and used to appropriately label the plot axes.  Note that for non-robust models the percentage variability explained will be the same as the percentage variability explained by the corresponding eigenvalues. 

The following describes the available plot option combinations, the first being the default:\cr
rplot = TRUE & qplot = TRUE & rowids = NULL, crosses (pch default) and variable names\cr
rplot = TRUE & qplot = FALSE & rowids = NULL, variable names only\cr
rplot = FALSE & qplot = TRUE & rowids = NULL, crosses (pch default) only\cr
rplot = FALSE & qplot = TRUE & rowids = TRUE, input matrix row numbers only\cr
rplot = FALSE & qplot = TRUE & rowids = FALSE, input matrix row identifiers\cr
rplot = TRUE & qplot = TRUE & rowids = TRUE, input matrix row numbers and variable names

Because functions \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}} or \code{\link{gx.robmva.closed}} require a matrix as input the sample IDs that may be in a data frame may be lost.  They may be re-inserted by copying dimnames(...)[[1]] from a data frame into the matrix.  Alternately, to plot in the component score space with Sample IDs, the scores can be recovered from the saved object, e.g., \code{save$rqscore[, 1]} and \code{save$rqscore[, 2]}, and used as the x- and y-coordinates in function \code{xyplot.tags} with the sample IDs from the source data frame. Appropriate plot and axis titling can be displayed by setting the function arguments \sQuote{by hand}.
}
\references{
Reimann, C., Filzmoser, P., Garrett, R. and Dutter, R., 2008. Statistical Data Analysis Explained: Applied Environmental Statistics with R. John Wiley & Sons, Ltd., 362 p.

Venables, W.N. and Ripley, B.D., 2001. Modern Applied Statistics with S-Plus, 3rd Edition, Springer,  501 p.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}}, \code{\link{gx.rotate}}, \code{\link{xyplot.tags}} }
\examples{
## Make test data available
data(sind)
data(sind.mat2open)
attach(sind)

## Save PCA results and display bi-plots
sind.save <- gx.mva.closed(sind.mat2open)
gx.rqpca.plot(sind.save)
gx.rqpca.plot(sind.save,
main = "Howarth & Sinding Larsen Stream Sediments\nclr transform",
pch = 4, rcol =2, qcol = 4)
gx.rqpca.plot(sind.save, rplot = TRUE, qplot = FALSE, rowids = NULL)
gx.rqpca.plot(sind.save, rplot = FALSE, qplot = TRUE, rowids = NULL)
gx.rqpca.plot(sind.save, rplot = FALSE, qplot = TRUE, rowids = TRUE)
gx.rqpca.plot(sind.save, rplot = TRUE, qplot = TRUE, rowids = FALSE,
rcol = 2, qcol = 4)
#
attach(sind)
xyplot.tags(sind.save$rqscore[, 1],sind.save$rqscore[, 2], ID, cex = 0.9)

## Clean-up and detach test data
rm(sind.save)
detach(sind)
}
\keyword{ hplot }

