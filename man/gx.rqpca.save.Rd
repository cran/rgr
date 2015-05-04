\name{gx.rqpca.save}
\alias{gx.rqpca.save}
\title{ Function to Save PCA Loadings and Scores as .csv files }
\description{
Function to save PCA matrices following computations by functions \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}} or \code{\link{gx.rotate}} as \sQuote{.csv} files.  By default appropriate files names are generated and the files saved in the Working Directory. 
}
\usage{
gx.rqpca.save(save, ifload = TRUE, ifcntrb = FALSE, ifscore = TRUE, 
	file = NULL)
}
\arguments{
  \item{save}{ a saved object from any of functions \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \cr\code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}} or \code{\link{gx.rotate}}. }
  \item{ifload}{ if \code{ifload = TRUE} the PC loadings will be saved.  The default is to save the PC loadings. }
  \item{ifcntrb}{ if \code{ifcntrb = TRUE} the percentage contribution of each variable (communality) to each PC will be saved.  The default is not to save this table. }
  \item{ifscore}{ if \code{ifscore = TRUE} the scores on the PCs will be saved.  The default is to save the PC scores.}
  \item{file}{ the name of the folder where the files are to be saved, a \sQuote{/} is appended before the synthesized file name, details of which are displayed on the current device.  If no file is specified, the files are saved to the Working Directory. }
}
\details{
By default the PCA matrices are saved to the Working Directory with a synthesized file name consisting of the name of the saved object from which the matrices are sourced and an appropriate identifying suffix and \sQuote{.csv}.  If a folder name has been provided in \code{file}, a \sQuote{/} is appended prior to the synthesized file name.  As each matrix is saved its file name is displayed on the current device.  When the saved object is from \code{gx.rotate} and is the input object both the original and Varimax loadings and PC scores will be saved by default.  
}
\value{
The last saved table, \code{table.rows}, is returned and may be saved as an object if required.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}}, \code{\link{gx.rotate}}. } 
\examples{
## Make test data available
data(sind.mat2open)

## Estimate and save robust PCA loadings and scores
sind.save <- gx.robmva.closed(sind.mat2open)
gx.rqpca.save(sind.save)

## NOTE: the example below is commented out
## Save PCA scores for future use
## gx.rqpca.save(sind.save, ifcntrb = TRUE, file = "D:\\R_work\\Project3")

## Clean-up
rm(sind.save)
}
\keyword{ IO }

