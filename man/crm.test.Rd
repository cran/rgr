\name{crm.test}
\alias{crm.test}
\docType{data}
\title{ National Geochemical Reconnaissance survey QA/QC data }
\description{
A subset of Control Reference Material (CRM) data from NGR surveys undertaken in 2000 and 2001.
}
\usage{data(crm.test)}
\format{
  A data frame with 97 observations on the following 2 variables.
  Unique identifiers are present in the data frame, use \code{dimnames(crm.test)[[1]]} to access or display them.
  \describe{
    \item{CRM}{ a code indicating the particular CRM analysed.  A factor variable with levels: \code{STSD-1}, \code{STSD-2}, \code{STSD-3}, \code{STSD-4}, \code{TILL-4}, \code{X}, \code{Y}, and \code{Z}. }
    \item{Cu}{ the copper determinations, mg/kg. }
  }
}
\details{
The \sQuote{value} of CRM is used to select the CRM data to be displayed, either by creating a specific subset, e.g., using \code{\link{gx.subset}}, or using the R construct \code{Cu[CRM=="X"]} in the call to function \code{crm.plot}.
}
\source{
Internal Geological Survey of Canada files.
}
\keyword{datasets}
