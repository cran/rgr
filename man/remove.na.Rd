\name{remove.na}
\alias{remove.na}
\title{ Remove and Count NAs }
\description{
Function to remove rows containing \code{NA}s from a data vector or matrix.  Also counts the number of rows remaining, the number of rows deleted, and in the case of a matrix the number of columns.  The results are returned in a list for subsequent processing in the calling function.
}
\usage{
remove.na(xx, iftell = TRUE)
}
\arguments{
  \item{xx}{ name of the vector or matrix to be processed. }
  \item{iftell}{ if \code{iftell = TRUE}, the default, the number of removed records is displayed. }
}
\value{
  \item{x}{ a data vector or matrix containing the elements in the vector or rows of the matrix \code{xx} without \code{NA}s. }
  \item{n}{ the length of \code{x}. }
  \item{m}{ the number of columns in the matrix \code{xx}, if \code{xx} is a vector the value \code{1} is returned. }
  \item{nna}{ the number of rows removed from \code{xx}. }
}
\details{
This function is called by many of the procedures in the \sQuote{rgr} package.  If one or more \code{NA}s are found the user is informed of how many.  In general a data frame will have been cleared of any <values represented by negative values or zeros prior to executing the procedure calling this function, see \code{\link{ltdl.fix.df}}, or \code{\link{ltdl.fix}} if a single vector is being processed.
}
\note{
The \code{iftell} \sQuote{switch} is used to suppress the display of the \code{NA} count in some summary statistics tables as the information is included in the table. 
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{ltdl.fix.df}}, \code{\link{where.na}} } 
\examples{
## remove NAs
xx <- c(15, 39, 18, 16, NA, 53)
temp.x <- remove.na(xx)
x <- temp.x$x[1:temp.x$n]

## to recover the other values returned
n <- temp.x$n
m <- temp.x$m
nna <- temp.x$nna

## to remove NA replacing a -9999 in kola.o
data(kola.o)
kola.o.fixed <- ltdl.fix.df(kola.o, coded = -9999)
temp.x <- remove.na(kola.o.fixed$pH)
x <- temp.x$x[1:temp.x$n]

## Clean-up
rm(xx)
rm(temp.x)
rm(x)
rm(n)
rm(m)
rm(nna)
rm(kola.o.fixed)
}
\keyword{ misc }
 

