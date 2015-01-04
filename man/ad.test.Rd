\name{ad.test}
\alias{ad.test}
\docType{data}
\title{ National Geochemical Reconnaissance survey QA/QC data }
\description{
A subset of analytical duplicate data from NGR surveys undertaken in 2000 and 2001.
}
\usage{data(ad.test)}
\format{
  A data frame with 160 observations on the following 2 variables.
  Unique identifiers are present in the data frame, use \code{dimnames(ad.test)[[1]]} to access or display them.
  \describe{
    \item{RS}{ the Replicate Status code. }
    \item{Cu}{ the copper determinations, mg/kg. }
  }
}
\details{
The Replicate Status code indicates the \sQuote{position} of the geochemical sample in the QA/QC structure.  RS = 8 indicates analytical duplicate, RS = 2 indicates the field duplicate, and RS = 1 indicates a routine regional coverage site that was \sQuote{duplicated}.  All other routine regional coverage sites are coded RS = 0.  In this file the data record for the analytical duplicate is followed by the data record for the physical sample the duplicate was split from.  The analytical duplicate may be split from any field sample, but preferrably from one of the field duplicates, permitting a more incisive investigation of sampling and analytical variability using function \code{\link{gx.triples.aov}}.
}
\source{
Internal Geological Survey of Canada files.
}
\keyword{datasets}
