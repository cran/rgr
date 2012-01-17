\name{var2fact}
\alias{var2fact}
\title{ Rearranges Data for Variables as Factors }
\description{
Rearranges data from a matrix or data frame into a matrix where data are tagged by their variables names as factors.  Used to concatenate data for display with functions \code{\link{tbplots.by.var}} and \code{\link{bwplots.by.var}}.
}
\usage{
var2fact(xmat)
}
\arguments{
  \item{xmat}{ name of the n by m data matrix or data frame to be processed. }
}
\value{
  \item{xx}{ a \code{n * m} by \code{2} matrix where each of the \code{n * m} rows contains a value that is paired with its variable name as a factor, see Note below. }
}
\details{
If the data for only some of the variables available in an attached matrix or data frame are to be processed use the \code{cbind} construct.  Thus, \code{temp.mat <- cbind(varname1, varname3, varname6, varname8)}.
} 
\note{
The \code{m} variables for \code{n} cases results in a \code{n * m} by \code{2} matrix, where \code{[1:n, 1]} contains the variable name for value\code{[1]} and \code{[1:n, 2]} contains the values for the n rows in the first column of \code{xmat}. Then rows \code{[n+1:2n, 1]} contain the variable name for value\code{[2]} and \code{[n+1:2n, 2]} contain the values for \code{n} rows in the second column, and so on.
}
\author{ Robert G. Garrett }
\examples{
## Display, convert data frame and display the result
data(ms.data1)
ms.data1
temp <- var2fact(ms.data1)
temp

## Clean-up
rm(temp)
}
\keyword{ misc }
 
