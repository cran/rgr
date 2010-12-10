\name{gx.subset}
\alias{gx.subset}
\title{ Extracts a Subset of Rows from a Dataframe }
\description{
The function extracts a subset of rows, and columns if required, from a dataframe and returns the subset as a new dataframe based on the criterion provided by the user.  Unused factor names are dropped.
}
\usage{
gx.subset(dfname, subset = TRUE)
}
\arguments{
  \item{dfname}{ name of the dataframe from which rows are to be extracted. }
  \item{subset}{ the criterion for selecting the subset (rows). }
}
\value{
  \item{data}{ a dataframe only containing the rows of the input dataframe where the criterion is met. }
}
\details{
The subset criterion can be \sQuote{complex} and be a combination of conditions, see Examples below.
}
\note{
This function is based on a script shared by Bill Venables on S-News, October 10, 1997.  As such it may pre-date the time that \code{\link{subset}} was added to the S-Plus library.  It is simple to use and has been retained.
}
\author{ William N. Venables }
\seealso{ \code{\link{subset}} }
\examples{
## Make test data available
data(kola.c)

## Make a subset of the data for Finland
finland.c <- gx.subset(kola.c, COUNTRY == "FIN")

## Make a subset of the data for rock type, LITHO, 82 occurring
## in Russia. Note that both COUNTRY and LITHO are factor variables 
russia.82 <- gx.subset(kola.c, COUNTRY == "RUS" & LITHO == 82)

## Make a subset of the data for Cu exceeding 50(ppm) in Norway
norway.cugt50 <- gx.subset(kola.c, COUNTRY == "NOR" & Cu >50)

## Make single element subsets, e.g. for use with function gx.cnpplts
## First locate the column in the dataframe where the element of
## interest is stored using dimnames(kola.c), we find that Be is the
## 19th column in the dataframe
dimnames(kola.c)
Norway <- gx.subset(kola.c,COUNTRY=="NOR")[,19]
Russia <- gx.subset(kola.c,COUNTRY=="RUS")[,19]
Finland <- gx.subset(kola.c,COUNTRY=="FIN")[,19]

## Clean up
rm(Norway)
rm(Russia)
rm(Finland)
}
\keyword{ misc }
 
