\name{crm.test.new}
\alias{crm.test.new}
\docType{data}
\title{ Simulated National Geochemical Reconnaissance survey QA/QC data }
\description{
A subset, N = 10, of simulated Control Reference Material (CRM) data for copper with mean 34.5 and SD 2.19.
}
\usage{data(crm.test.new)}
\format{
  A data frame with 10 observations on the following 2 variables.
  Unique identifiers are present in the data frame, use \code{dimnames(crm.test)[[1]]} to access or display them.
  \describe{
    \item{CRM}{ a code indicating the particular CRM simulated.  A factor variable with a single level: \code{X}. }
    \item{Cu}{ the simulated copper determinations, mg/kg. }
  }
}
\details{
 A set of simulated copper data for demonstrating function \code{crm.plot.new}, for details of usage see \code{\link{crm.plot.new}}.
}
\source{
rnorm(10, 34.5, 2.19)
}
\keyword{datasets}
