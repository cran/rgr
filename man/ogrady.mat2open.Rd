\name{ogrady.mat2open}
\alias{ogrady.mat2open}
\docType{data}
\title{ Lithogeochemical Data Set from the O'Grady Pluton, NWT, 1970 }
\description{
The major and minor elements (see below) from the data for the O'Grady pluton, NWT, (NTS map sheet 105I), see \code{\link{ogrady}} for further details.  Additionally, the UTM coordinates and lithology classifications have been removed from the \code{ogrady} data frame.  This data set is provided so that users can investigate different approaches to the closure problem using the various functions in package \sQuote{rgr}.
}
\usage{data(ogrady.mat2open)}
\format{
  A matrix with 110 observations for the following 10 variables:
  \describe{
    \item{Si}{ silicon (mg/kg) in granitoid (Li-T OES). }
    \item{Al}{ aluminium (mg/kg) in granitoid (Li-T OES). }
    \item{Fe}{ iron mg/kg) in granitoid (Li-T HNO3 AAS). }
    \item{Mg}{ magnesium (mg/kg) in granitoid (Li-T OES). }
    \item{Ca}{ calcium (mg/kg) in granitoid (Li-T OES). }
    \item{Na}{ sodium (mg/kg) in granitoid (Li-T HNO3 AAS). }
    \item{K}{ potassium (mg/kg) in granitoid (Li-T HNO3 AAS). }
    \item{Ti}{ titanium (mg/kg) in granitoid (Li-T OES). }
    \item{Mn}{ manganese (mg/kg) in granitoid (Li-T OES). }
    \item{Ba}{ barium (mg/kg) in granitoid (Li-T OES). }
  }
}
\source{
Garrett, R.G., 1992. Lithogeochemical data release, major and trace elements in Cretaceous granitoid rocks in the Yukon Territory and adjoining parts of the N.W.T. (95E, L, 105H, I, J, K, L, M, N, O, P, 106D, 115P, 116D). Geological Survey of Canada Open File 2479, digital data. 
}
\references{
Garrett, R.G., 1972. Regional geochemical study of Cretaceous acidic igneous rocks in the northern Canadian Cordillera as a tool for broad mineral exploration.  in Proc. 4th International Geochemical Exploration Symp., Geochemical Exploration 1972 (Ed. M.J. Jones). Institute of Mining and Metallurgy, London, pp. 203-219.

Garrett, R.G., 1988. IDEAS - An interactive computer graphics tool to assist the exploration geochemist. In Current Research Part F, Geological Survey of Canada Paper 88-1F, pp. 1-13.
}
\seealso{ \code{\link{ogrady}} }
\keyword{datasets}
