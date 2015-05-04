\name{gx.mvalloc.print}
\alias{gx.mvalloc.print}
\title{ Function to display the results of Multivariate Allocation }
\description{ Function to extract and display the results from the saved object from \code{gx.mvalloc} or \cr\code{gx.mvalloc.closed}.  The function displays on the current device only those individuals (observations, cases or samples) whose predicted probability of reference group membership (typicality) was less than the value provided, \code{pcrit}, for all reference groups, i.e. the outliers.  Alternately, the results can be saved as a \sQuote{.csv} file for viewing with a spreadsheet program and any subsequent post-processing.
}
\usage{
gx.mvalloc.print(save, ifprint = TRUE, unalloc = TRUE, file = NULL)
}
\arguments{
  \item{save}{ an object saved from \code{\link{gx.mvalloc}}. }
  \item{ifprint}{ by default the \sQuote{outliers}, i.e. individuals classified into group \sQuote{zero}, are displayed on the current device.  The display consists of the input matrix row numbers together with the predicted probabilities of reference group membership for the \code{1:kk} reference groups.  To suppress the display set \code{ifprint = FALSE}. }
  \item{unalloc}{ by default, \code{unalloc = TRUE}, individuals that were not allocated to one of the reference groups are displayed.  To suppress displaying these individuals, set \code{unalloc = FALSE}. } 
  \item{file}{ the name of the folder where the file is to be saved, a \sQuote{/} is appended before the synthesized file name, details of which are displayed on the current device.  If no file is specified, the files are saved to the Working Directory, see Details below. }
}
\details{
The full table of Mahalanobis distances and group membership probabilities may be saved as a \sQuote{.csv} file.  If a folder name has been provided in \code{file}, a \sQuote{/} is appended prior a file name synthesized from the name of the saved object concatenated with \sQuote{_mvalloc.csv}, see Example below.  If \code{file = ""} or \code{file = " "} the file will be saved to the Working Directory.  The file name for the \code{.csv} file is displayed on the current device, and all other output is suppressed. The output file can be inspected with spread sheet software or a viewer of the user's choice.
}
\note{
Included in the display on the current device are the names of the \code{kk} reference group objects supplied to \code{\link{gx.mvalloc}} together with the value of \code{pcrit}.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.mvalloc}}, \code{\link{gx.mvalloc.closed}} }
\examples{
## Make test data available
data(ogrady)
attach(ogrady)
ogrady.grdr <- gx.subset(ogrady, Lith == "GRDR")
ogrady.grnt <- gx.subset(ogrady, Lith == "GRNT")
## Ensure all data are in the same units (mg/kg)
ogrady.grdr.2open <- ogrady.grdr[, c(5:14)]
ogrady.grdr.2open[, 1:7] <- ogrady.grdr.2open[, 1:7] * 10000
ogrady.grnt.2open <- ogrady.grnt[, c(5:14)]
ogrady.grnt.2open[, 1:7] <- ogrady.grnt.2open[, 1:7] * 10000
ogrady.2open <- ogrady[, c(5:14)]
ogrady.2open[, 1:7] <- ogrady.2open[, 1:7] * 10000 

## Create reference data sets
ogrady.grdr.save <- gx.md.gait(ilr(as.matrix(ogrady.grdr.2open)),
mcdstart = TRUE)
ogrady.grnt.save <- gx.md.gait(ilr(as.matrix(ogrady.grnt.2open)),
mcdstart = TRUE)

## Allocate all O'Grady granitoids
ogrady.mvalloc <- gx.mvalloc(pcrit = 0.02, ilr(as.matrix(ogrady.2open)),
ogrady.grdr.save, ogrady.grnt.save)

## Display list of outliers
gx.mvalloc.print(ogrady.mvalloc)

## Save allocations as a .csv file, not executed
## gx.mvalloc.print(ogrady.mvalloc, ifprint = FALSE, file = "D://R_work//Project3")

## Clean-up and detach test data
rm(ogrady.grdr)
rm(ogrady.grnt)
rm(ogrady.grdr.2open)
rm(ogrady.grnt.2open)
rm(ogrady.2open)
rm(ogrady.grdr.save)
rm(ogrady.grnt.save)
rm(ogrady.mvalloc)
detach(ogrady)
} 
\keyword{ print }

