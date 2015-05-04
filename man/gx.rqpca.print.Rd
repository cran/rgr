\name{gx.rqpca.print}
\alias{gx.rqpca.print}
\title{ Function to Display PCA Loadings and Scores }
\description{
Function to display PCA matrices following computations by functions \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}} or \code{\link{gx.rotate}}.  The user may optionally display the loadings (default), the percentage contribution of the variables to the loadings, i.e. communalities (not default), and the scores on the PCs (default). 
}
\usage{
gx.rqpca.print(save, ifload = TRUE, ifcntrb = FALSE, ifscore = TRUE)
}
\arguments{
  \item{save}{ a saved object from any of functions \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \cr\code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}} or \code{\link{gx.rotate}}. }
  \item{ifload}{ if \code{ifload = TRUE} the PC loadings are displayed.  The default is to display the PC loadings. }
  \item{ifcntrb}{ if \code{ifcntrb = TRUE} the percentage contribution of each variable (communality) to each PC is displayed.  The default is not to display this table. }
  \item{ifscore}{ if \code{ifscore = TRUE} the scores on the PCs are displayed.  The default is to display the PC loadings.}
}
\details{
By default the PCA loadings and scores on the PCs are displayed on the current device.  Optionally the percentage contribution, communality, of each variable to each PC may also be displayed.  Additionally a table of cumulative percent contributions, communalities, is displayed to assist in deciding how many components to retain for rotation or further study.  When the saved object from \code{gx.rotate} is the input object both the original and Varimax loadings and scores will be displayed by default.  
}
\value{
The last displayed table, \code{table.rows}, is returned and may be saved as an object if required.
}
\note{
For large tables, particularly scores, it may be preferrable to save them as \sQuote{.csv} files using \code{\link{gx.rqpca.save}} for inspection and later use.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.rqpca.save}}, \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}}, \code{\link{gx.rotate}}. } 
\examples{
## Make test data available
data(sind.mat2open)

## Estimate and display robust PCA loadings and scores
sind.save <- gx.robmva.closed(sind.mat2open)
gx.rqpca.print(sind.save, ifcntrb = TRUE)

## Clean-up
rm(sind.save)
}
\keyword{ print }

