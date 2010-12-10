\name{df.test}
\alias{df.test}
\title{ Check for the existence of a Dataframe }
\description{
A utility function to determine if a dataframe is attached, or exists in the working directory. If the dataframe exists the names of the variables are displayed.
}
\usage{
df.test(dfname, x = NULL)
}
\arguments{
  \item{dfname}{ name of a data frame. }
  \item{x}{ setting \code{x} to an expected variable name in the dataframe results in the variable's length, i.e. number of observations, being displayed if it is present. }
}
\details{
Based on a function shared on S-News.
}
\author{ Unkown }
\seealso{ \code{\link{search}}, \code{\link{ls}} }
\examples{
## Make test data available
data(kola.o)

## Check that the dataframe kola.o is available
df.test(kola.o)

## Check that kola.o is available and that the variable Cu is present
df.test(kola.o, x = Cu)

}
\keyword{ misc }

