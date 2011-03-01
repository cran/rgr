\name{gx.rqpca.print}
\alias{gx.rqpca.print}
\title{ Function to Display PCA Loadings and Scores }
\description{
Function to display PCA matrices following computations by functions \code{\link{gx.mva}}, \code{\link{gx.robmva}}, \cr\code{\link{gx.robmva.closed}} or \code{\link{gx.rotate}}.  The user may optionally display the loadings (default), the percentage contribution of the variables to the loadings (not default), and the scores on the PCs (default).  Optionally the entire table of PC scores may be saved as a \sQuote{csv} file for future use. 
}
\usage{
gx.rqpca.print(save, ifload = TRUE, ifcntrb = FALSE, ifscore = TRUE, 
	file = NULL)
}
\arguments{
  \item{save}{ a saved object from any of functions \code{gx.mva}, \code{gx.robmva}, \cr\code{gx.robmva.closed} or \code{gx.rotate}. }
  \item{ifload}{ if \code{ifload = TRUE} the PC loadings are displayed.  The default is to display the PC loadings. }
  \item{ifcntrb}{ if \code{ifcntrb = TRUE} the percentage contribution of each variable to each PC is displayed.  The default is not to display this table. }
  \item{ifscore}{ if \code{ifscore = TRUE} the scores on the PCs are displayed.  The default is to display the PC loadings.}
  \item{file}{ the file name for saving the function output in the R working directory, see Details below. }
}
\details{
By default the PCA laodings and scores on the PCs are displayed on the current device.  Optionally the percentage contribution of each variable to each PC may also be displayed.  When the saved object from \code{gx.rotate} is the input object both the original and Varimax loadings and PC scores will be dispslayed by default.  The last table displayed by the function may be saved as a \sQuote{csv} file in the working directory.  Note, the \sQuote{.csv} extension is appended in the function.  See example below.
}
\value{
The last displayed table, \code{table.rows}, on the current device is returned and may be saved as an object if required.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.mva}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}}, \code{\link{gx.rotate}}. } 
\examples{
## Make test data available
data(sind)
attach(sind)
sind.mat <- as.matrix(sind[, -c(1:3)])
## Ensure all data are in the same units (mg/kg)
sind.mat2open <- sind.mat
sind.mat2open[, 2] <- sind.mat2open[, 2] * 10000

## Estimate and display robust PCA loadings and scores
sind.save <- gx.robmva.closed(sind.mat2open)
gx.rqpca.print(sind.save)

## Save display for future use
gx.rqpca.print(sind.save, file = "sind.save.rob.pca")

## Clean-up and detach test data
rm(sind.mat)
rm(sind.mat2open)
rm(sind.save)
detach(sind)
}
\keyword{ print }

