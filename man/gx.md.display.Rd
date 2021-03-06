\name{gx.md.display}
\alias{gx.md.display}
\title{ Function to Display Membership Probabilities and Other Relevant Data }
\description{
Function to display the Mahalanobis distances (MDs) and predicted probabilities of membership (ppm or p_gm), together with other relevant data, following computations by functions \code{\link{gx.md.gait}}, \code{\link{gx.md.gait.closed}}, \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}} or \code{\link{gx.robmva.closed}}.  The user may select the predicted probability of membership below which the results are displayed.  A simpler presentation is available with \code{\link{gx.md.print}}.  Optionally the entire generated table may be saved as a \sQuote{.csv} file for future use. 
}
\usage{
gx.md.display(xx, pcut = 0.1, ifprint = TRUE, file = NULL)
}
\arguments{
  \item{xx}{ the data matrix to be displayed, see Details below. }
  \item{pcut}{ the probabilitity of group membership below which records will be displayed on the current device in ascending order of membership probability, i.e. the most outlying individuals first. }
  \item{ifprint}{ by default, \code{ifprint = TRUE}, the most outlying individuals are displayed displayed on the current device, see \code{pcut} above. To suppress the display set \code{pcut = FALSE}. }
  \item{file}{ the name of the folder where the file is to be saved, a \sQuote{/} is appended before the synthesized file name, details of which are displayed on the current device.  If no folder is specified, the files are saved to the Working Directory, see Details below.  The default, \code{file = NULL}, results in no file being saved. }
}
\details{
The data frame from which the matrices were derived from for use by the above listed functions must be attached if such items as (numeric) sample IDs, coordinates and data values are to be displayed.  Those numeric variables to be displayed must be appended in a \code{cbind} construct following the \sQuote{MDs} and \sQuote{ppms} extracted from the saved objects from the above listed functions.  For example, \code{xx <- cbind(save.sind$md, save.sind$ppm, E, N, Zn, Cu)}.  If row names were present in the original data frame they will have been carried forward in any saved object from an \sQuote{rgr} function and will be displayed in the first column of the table. 

The table generated by the function may be saved as a \sQuote{.csv} file in a folder of the user's choice or in the working directory, with the \sQuote{.csv} being appended in the function, see Example below.  If \code{file = ""} or \code{file = " "} the file is saved to the Working Directory.  The file name is synthesized as \sQuote{\\MD_display.csv} and appended to the folder name.
}
\value{
  \item{table.rows}{ The displayed table is returned and may be saved as an object if required.  It will contain the information passed to the function as \code{xx} sorted by MD and with appropriate column headings. }
}
\note{
This function is similar in purpose to \code{\link{gx.mvalloc.print}} for displaying multivariate outliers, however, it operates on a single population. 
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.md.gait}}, \code{\link{gx.md.gait.closed}}, \code{\link{gx.mva}}, \code{\link{gx.mva.closed}}, \code{\link{gx.robmva}}, \code{\link{gx.robmva.closed}}, \code{\link{gx.mvalloc}}, \code{\link{gx.mvalloc.print}}, \code{\link{gx.md.print}}. } 
\examples{
## Make test data available
data(sind.mat2open)
data(sind)
attach(sind)
## data frame sind attached to provide access to row IDs

## Estimate and default display robust Mahalanobis distances
sind.save <- gx.robmva.closed(sind.mat2open)
xx <- cbind(sind.save$md, sind.save$ppm, E, N, Cu, Zn)
gx.md.display(xx)

## Save display for future use in the Working Directory
gx.md.display(xx, file = " ")

## Clean-up
rm(sind.save)
rm(xx)
detach(sind)
}
\keyword{ print }

