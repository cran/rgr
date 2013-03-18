\name{df.test}
\alias{df.test}
\title{ Check for the Existence of a Data Frame }
\description{
A utility function to determine if a data frame is attached, or exists in the working directory. If the data frame exists the names of the variables are displayed together with the data frame dimensions.
}
\usage{
df.test(dfname)
}
\arguments{
  \item{dfname}{ name of a data frame. }
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
df.test(kola.o)

}
\keyword{ misc }

