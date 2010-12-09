\name{gx.sort.df}
\alias{gx.sort.df}
\title{ Function to Multi-Column Sort a Dataframe }
\description{ 
Function to sort a dataframe on any combination of numerical values or factors in any combination of ascending or descending orders.  If the function is run as \code{temp <- gx.sort.df(formula, dfname)} the sorted data are not displayed, but retained in \code{temp} for subsequent use or display.
}
\usage{
gx.sort.df(formula, dfname)
}
\arguments{
  \item{formula}{ a \sQuote{formula} defining the variables to be used in the sort and whether the sort for each is to be in ascending or descending order.  The sort order is from left to right in the formula.  See details and examples below. }
  \item{dfname}{ the name of the dataframe to be sorted. }
}
\details{
The sort is controlled by a text string in the form of a \sQuote{formula}, so \code{~var1+var2} will sort in ascending order of var1, and then within equal values for var1 in ascending order of var2. A preceding \code{+} or \code{-} before a column name indicates a sort in ascending or descending order, respectively.

The function also works if \code{formula} and \code{dfname} are reversed in the function call.
}
\author{  Kevin Wright with some ideas from Andy Liaw\cr
Shared on S-News and R-help in September 2004. }
\seealso{ \code{\link{gx.sort}} }
\examples{
## Make test data available
data(kola.c)
attach(kola.c)
names(kola.c)

## Create a small test data set for ID (1), COUNTRY (4),
## As (17), Co (21), Cu (23) and Ni (28)
test<-kola.c[1:25, c(1,4,17,21,23,28)]

## Sort test data into ascending order on the value of Ni
gx.sort.df(~Ni, test)
temp <- gx.sort.df(test, ~Ni)
temp

## Sort test data by Country and descending order of As
gx.sort.df(test, ~COUNTRY-As)

## Sort test data by Country and descending order of both
## As and Ni
gx.sort.df(test, ~COUNTRY-As-Ni)

## Clean up and detach test data
rm(test)
rm(temp)
detach(kola.c)
}
\keyword{ misc }
\keyword{ arith }

