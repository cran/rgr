\name{fix.test}
\docType{data}
\alias{fix.test}
\title{ Test Data for Function ltdl.fix.df }
\description{
A set of test data to demonstrate how negative values are changed to half their positive value.  Optionally numeric coded values representing missing data and/or zero values may be replaced by \code{NA}s.

The \code{.csv} file was read without deleting ID, the row (observation) identifier in the first column, from the header record.  Therefore the character row ID is saved as a factor variable.  If ID had been deleted from the header record the row ID would have been stored as dimnames(fix.test)[[1]].
}
\usage{fix.test}
\format{ A data frame containing 15 rows and 5 columns (3 factors, one is ID, and 2 numeric). }
\seealso{ \code{\link{fix.test.asis}} }
\keyword{ datasets }








