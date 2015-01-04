\name{fix.test.asis}
\docType{data}
\alias{fix.test.asis}
\title{ Test Data for Function ltdl.fix.df }
\description{
A set of test data to demonstrate how negative values are changed to half their positive value.  Optionally numeric coded values representing missing data and/or zero values may be replaced by \code{NA}s.

The \code{.csv} file was read without deleting ID, the row (observation) identifier in the first column, from the header record, and with \code{as.is} set to \code{as.is = c(1)}.  Therefore the character row ID is saved as a character variable, not as a factor variable.  If ID had been deleted from the header record the row ID would have been stored as dimnames(fix.test)[[1]].
}
\usage{fix.test}
\format{ A data frame containing 15 rows and 5 columns (1 character, 2 factors, and 2 numeric). }
\seealso{ \code{\link{fix.test}} }
\keyword{ datasets }








