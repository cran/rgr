\name{triples.test2}
\alias{triples.test2}
\docType{data}
\title{ North American Geochemical Soil Landscapes Project QA/QC data}
\description{
A small data set of regional and field duplicate samples from the Maritimes 2007 NAmSGLs survey.  The analyses are for <2 mm unmilled C-horizon soil samples, and the determinations were made by ICP-OES or -MS following an Aqua Regia digestion. 
}
\usage{data(triples.test2)}
\format{
  A dataframe with 186 observations for the following 3 variables:
  \describe{
    \item{ID}{ the NAmSGLs unique site identifier. }
    \item{RS}{ the Replicate Status code. }
    \item{Ba_ppm}{ the barium analyses, mg/kg }
  }
}
\details{
The replicate status codes indicate the \sQuote{position} of the geochemical sample in the QA/QC structure.  RS = 2 indicates the field duplicate, and RS = 1 indicates a routine regional coverage site that was \sQuote{duplicated}.  All other routine regional coverage sites are coded RS = 0. }
\source{
The Geological Survey of Canada, see Open File 6433, from which this data subset for barium (Ba) was extracted.
}
\references{
Friske, P.W.B., Ford, K.L. and McNeil, R.J., 2011. Soil Geochemical, Mineralogical, Radon and Radiometric Data from the 2007 North American Soil Geochemical Landscapes Project in New Brunswick, Nova Scotia and Prince Edward Island.  Geological Survey of Canada, Open File 6433.
}
\keyword{datasets}
