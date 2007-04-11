\name{dftest}
\alias{dftest}
\title{ Check for the Existence of a Data Frame }
\description{
A utility function to determine if a data frame is attached, or exists in the working directory. If the data frame exists the names of the variables are displayed.
}
\usage{
dftest(dfname, x = NULL)
}
\arguments{
  \item{dfname}{ name of a data frame. }
  \item{x}{ setting \code{x} to an expected variable name in the data frame results in the variable's length, i.e., number of observations, being displayed if it is present. }
}
\details{
Based on a function shared on S-News.
}
\author{ Unkown }
\seealso{ \code{\link{search}}, \code{\link{ls}} }
\examples{
## Make test data available
data(kola.o)

## Check that the data frame kola.o is available
dftest(kola.o)

## Check that kola.o is available and that the variable Cu is present
dftest(kola.o, x = Cu)

}
\keyword{ misc }

