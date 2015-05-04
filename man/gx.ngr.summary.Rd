\name{gx.ngr.summary}
\alias{gx.ngr.summary}
\title{ Saves a NGR Report Summary Statistics Table to a .csv File }
\description{
Prepares a \code{.csv} file with the standard set of NGR Report summary statistics for further processing with a spread sheet program.  The table includes: N (data set size), number of NAs, mean, SD, skew, CV\%, geometric mean, median, MAD, robust CV\%, and the miniumum and maximum values and 17 intermediate percentiles.
}
\usage{
gx.ngr.summary(xmat, vars, banner = deparse(substitute(xmat)),
	file = NULL)
}
\arguments{
  \item{xmat}{ name of the matrix or data frame. }
  \item{vars}{ the indices, or names (see Example), of the columns of the matrix or data frame for the variables whose summary statistics are to be displayed. }
  \item{banner}{ by default the character string for \code{xmat}, the input matrix, is used in the file name.  An alternate title can be displayed with \code{banner = "text string"}, see Details below. }
  \item{file}{ the name of the folder where the files are to be saved, a \sQuote{/} is appended before the synthesized file name, details of which are displayed on the current device.  If no file is specified, the files are saved to the Working Directory. }
}
\details{
The file name is synthesized by concatenating the input matrix name and \sQuote{_NGR_Summary.csv}, and prefixed by sQuote{/}.  If a data frame is the data source, the synthesize file name will be \sQuote{/dataframe_NGR_Summary.csv}, as it is not possible to obtain the text string for the data frame name from \code{xmat}.  In such cases the user should rename the \code{.csv} file mpre appropriately after it has been saved.  If a folder name has been provided in \code{file}, for example, \code{file = "D://R_work//Project3"}, a \sQuote{/} is appended prior to the synthesized file name.

The file name for the \code{.csv} file is displayed on the current device, and all other output is suppressed. The output file can be inspected with spread sheet software or a viewer of the user's choice.
}
\note{
Any less than detection limit values represented by negative values, or zeros or other numeric codes representing blanks in the data, must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector will be removed prior to computation in function \code{gx.stats}.  Display of the number of \code{NAs} found by function \code{remove.na} is suppressed in \code{remove.na} as the information is included in the display from this function that is prepared by \code{\link{gx.ngr.stats}}.

For a more extensive summary statistics display a variable at a time, see \code{\link{gx.summary2}}.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.ngr.stats}}, \code{\link{gx.stats}}, \code{\link{gx.ngr.skew}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}}, \code{\link{gx.summary1}}, \code{\link{gx.summary2}} }
\examples{
## Make test data available
data(sind.mat2open)

## Prepare and export a .csv file 
gx.ngr.summary(sind.mat2open)
}
\keyword{ univar }

