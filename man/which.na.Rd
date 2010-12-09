\name{which.na}
\alias{which.na}
\title{ Identify Rows with NAs }
\description{
Function to display the numbers of rows containing NAs in a vector.  The function can also be used to remove NAs from a vector.
}
\usage{
which.na(x)
}
\arguments{
  \item{x}{ name of the vector to be processed. }
}
\value{
  \item{whichna}{ a vector containingg the numbers of the rows in \code{x} containing \code{NA}s. }
}
\note{
This function is based on the S-Plus function which.na and is useful in finding the location of NAs in a data set.  While \code{\link{remove.na}} removes NAs it does not identify their positions.  A vector is returned that can be used to remove NAs, see example below. 
}
\author{ S-Plus team and Robert G. Garrett }
\seealso{ \code{\link{remove.na}} } 
\examples{
## Identify rows with NAs
xx <- c(15, 39, 18, 16, NA, 53)
which.na(xx)

## To use which.na to remove NAs, method 1
xx
temp <- which.na(xx)
temp
xxx <- xx[-temp]
xxx

## To use which.na to remove NAs, method 2
xx
xxx <- xx[-which.na(xx)]
xxx

## Clean up
rm(xx)
rm(xxx)
rm(temp)
}
\keyword{ misc }
 

