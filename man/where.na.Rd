\name{where.na}
\alias{where.na}
\title{ Identify Vector Elements or Data Frame/Matrix Rows with NAs }
\description{
Function to display the positions of elements in a vector containing NAs, or the numbers of rows in a data frame or matrix containing one or more NAs.  The function can also be used to remove NAs.
}
\usage{
where.na(x)
}
\arguments{
  \item{x}{ name of the vector or matrix/data frame to be processed. }
}
\value{
  \item{whichna}{ a vector containing the indices of the positions in \code{x} containing \code{NA}s. }
}
\note{
This function is based on the S-Plus function \code{which.na} and is useful in finding the location of NAs in a data set.  While \code{\link{remove.na}} removes NAs it does not identify their positions.  A vector is returned that can be used to remove NAs, see example below.

Remember, a matrix is also a vector with the columns occurring sequentially.
}
\author{ S-Plus team and Robert G. Garrett }
\seealso{ \code{\link{remove.na}} } 
\examples{
## Identify rows with NAs
xx <- c(15, 39, 18, 16, NA, 53)
where.na(xx)

## To use where.na to remove NAs, method 1
xx
temp <- where.na(xx)
temp
xxx <- xx[-temp]
xxx

## To use where.na to remove NAs, method 2
xx
xxx <- xx[-where.na(xx)]
xxx

## Clean-up
rm(xx)
rm(xxx)
rm(temp)
}
\keyword{ misc }
 

