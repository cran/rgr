\name{gx.md.print}
\alias{gx.md.print}
\title{ Function to Display Membership Probabilities }
\description{
Function to display the Mahalanobis distances (MDs) and predicted probabilities of membership (ppm) following computations by functions \code{\link{gx.md.gait}}, \code{\link{gx.md.gait.closed}}, \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}} or \code{\link{gx.robmva.closed}}.  The user may select the predicted probability of membership below which the results are displayed.  Alternately the Mahalanobis distances and group membership probabilities may be saved as a \sQuote{.csv} file for future use. 
}
\usage{
gx.md.print(save, pcut = 0.1, ifprint = TRUE, file = NULL)
}
\arguments{
  \item{save}{ a saved object from any of functions \code{gx.md.gait}, \code{gx.md.gait.closed}, \code{gx.mva}, \code{gx.mva.closed}, \code{gx.robmva}, or \code{gx.robmva.closed}. }
  \item{pcut}{ the probabilitity of group membership below which records will be displayed on the current device in ascending order of membership probability, i.e. the most outlying individuals first. }
  \item{ifprint}{ by default, \code{ifprint = TRUE}, the most outlying individuals are displayed displayed on the current device, see \code{pcut} above. To suppress the display set \code{pcut = FALSE}. }
  \item{file}{ the name of the folder where the file is to be saved, a \sQuote{/} is appended before the synthesized file name, details of which are displayed on the current device.  If no folder is specified, the files are saved to the Working Directory, see Details below.  The default, \code{file = NULL}, results in no file being saved. }
}
\details{
The Mahanalobis distances, the membership probabilities and input matrix row numbers or IDs are extracted from the saved object and sorted in increasing order of probabilities of group membership for display on the current device.  The full table of Mahalanobis distances and group membership probabilities may be saved as a \sQuote{.csv} file.  If a folder name has been provided in \code{file}, a \sQuote{\\} is appended prior to a file name synthesized from the name of the saved object concatenated with \sQuote{_MDs.csv}.  If \code{file = ""} or \code{file = " "} the file will be saved to the Working Directory.  Details of the file name are displayed on the current device, see Example below.  For an alternative display see \code{\link{gx.md.display}}.
}
\value{
  \item{table.rows}{The last table processed is returned and may be saved as an object if required. }
}
\note{
This function is similar in purpose to \code{\link{gx.mvalloc.print}} for displaying multivariate outliers, however, it operates on a single population. 
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.md.gait}}, \code{\link{gx.md.gait.closed}}, \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}}, \code{\link{gx.md.display}}, \code{\link{gx.mvalloc}}, \code{\link{gx.mvalloc.print}}. } 
\examples{
## Make test data available
data(sind.mat2open)

## Estimate and display robust Mahalanobis distances
sind.save <- gx.robmva.closed(sind.mat2open)
gx.md.print(sind.save, pcut = 0.3)

## Save display for future use, not executed
## gx.md.print(sind.save, ifprint = FALSE, file = "D://R_work//Project3")

## Clean-up
rm(sind.save)
}
\keyword{ print }

