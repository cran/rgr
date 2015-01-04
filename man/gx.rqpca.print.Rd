\name{gx.rqpca.print}
\alias{gx.rqpca.print}
\title{ Function to Display PCA Loadings and Scores }
\description{
Function to display PCA matrices following computations by functions \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}} or \code{\link{gx.rotate}}.  The user may optionally display the loadings (default), the percentage contribution of the variables to the loadings, i.e. communalities (not default), and the scores on the PCs (default).  Optionally the entire table of PC scores may be saved as a \sQuote{.csv} file for future use. 
}
\usage{
gx.rqpca.print(save, ifload = TRUE, ifcntrb = FALSE, ifscore = TRUE, 
	file = NULL)
}
\arguments{
  \item{save}{ a saved object from any of functions \code{gx.mva}, \code{gx.mva.closed}, \cr\code{gx.robmva}, \code{gx.robmva.closed} or \code{gx.rotate}. }
  \item{ifload}{ if \code{ifload = TRUE} the PC loadings are displayed.  The default is to display the PC loadings. }
  \item{ifcntrb}{ if \code{ifcntrb = TRUE} the percentage contribution of each variable (communality) to each PC is displayed.  The default is not to display this table. }
  \item{ifscore}{ if \code{ifscore = TRUE} the scores on the PCs are displayed.  The default is to display the PC loadings.}
  \item{file}{ the file name for saving the function output in the R working directory, see Details below. }
}
\details{
By default the PCA loadings and scores on the PCs are displayed on the current device.  Optionally the percentage contribution, communality, of each variable to each PC may also be displayed.  Additionally a table of cumulative percent contributions, communalities, is displayed to assist in deciding how many components to retain for rotation or further study.  When the saved object from \code{gx.rotate} is the input object both the original and Varimax loadings and PC scores will be dispslayed by default.  The last table displayed by the function may be saved as a \sQuote{csv} file in the working directory.  Note, the \sQuote{.csv} extension is appended in the function.  See example below.
}
\value{
The last displayed or saved table, \code{table.rows}, is returned and may be saved as an object if required.
}
\note{
For large tables of scores all options may be set to \code{FALSE} to suppress table output to the display device, and the PC scores or rotated PC scores will be saved as a \sQuote{.csv} file as long as a text string is defined for \code{file}.  If \code{file} is left undefined the function will fail with the message \dQuote{object \sQuote{table.rows} not found}.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}}, \code{\link{gx.rotate}}. } 
\examples{
## Make test data available
data(sind.mat2open)

## Estimate and display robust PCA loadings and scores
sind.save <- gx.robmva.closed(sind.mat2open)
gx.rqpca.print(sind.save, ifcntrb = TRUE)

## Save PCA scores for future use
gx.rqpca.print(sind.save, file = "sind.rob.pca.scores")

## Clean-up
rm(sind.save)
}
\keyword{ print }

