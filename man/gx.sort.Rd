\name{gx.sort}
\alias{gx.sort}
\title{ Function to Single Column Sort a Matrix or Data Frame }
\description{ 
Function to sort a matrix or data frame by the value in a column.  On exit the function displays the sorted data.  Any NAs in the sort column are sorted to beyond the greatest value.  If the function is run as \code{temp <- gx.sort(x, ncol)} the sorted data are not displayed, but retained in \code{temp} for subsequent use or display.
}
\usage{
gx.sort(x, col = 1, reverse = FALSE)
}
\arguments{
  \item{x}{ the matrix or data frame to be sorted. }
  \item{col}{ a column number, the value of which will be used to sort the matrix or data frame }
  \item{reverse}{ the default is to sort in ascending order of the value in column \code{col}.  If a descending order sort is required, set \code{reverse = TRUE}. }
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.sort.df}} }
\examples{
## Make test data available
data(sind)
attach(sind)

## Sort data frame sind into ascending order on the value
## of column 4, Zn
gx.sort(sind, 4)

## Sort data frame sind into descending order on the value
## of column 4, Zn
gx.sort(sind, 4, reverse = TRUE)

## Sort only the geochemical data in data frame sind into
## descending order on the value of column 4, Zn.  Note
## that sind[, -c(1:3)] moves the old column 4 to
## position 1
gx.sort(sind[, -c(1:3)], 1, reverse = TRUE)

## Detach test data
detach(sind)
}
\keyword{ misc }
\keyword{ arith }

