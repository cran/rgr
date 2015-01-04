\name{gx.hypergeom}
\alias{gx.hypergeom}
\title{ Compute Probabilities for Target Recognition }
\description{
The hypergeometric distribution is used to infer if the number of anomalous sites along a traverse reliably reflect the presence of the dispersion pattern from a known mineral occurrence.  The function displays the probability of the observed outcome could be due to chance alone.
}
\usage{
gx.hypergeom(tt, aa, kk, xx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tt}{ total number of sites along a traverse. }
  \item{aa}{ number of sites that a priori should be anomalous. }
  \item{kk}{ total number of > threshold sites. }
  \item{xx}{ number of the \code{aa} that are > threshold.}
}
\details{
See Stanley (2003) for details, the examples below reproduce the results in Table 1 and Table 2.
}
\note{
Effectively, the hypothesis being tested is that the pattern of above threshold (see \code{\link{fences}}), sites coincides the the expected dispersion pattern from a known mineral occurrence.  This requires that the geochemist uses knowledge of the dispersion processes active along the traverse, both chemical and mechanical, to predict an expected dispersion pattern.
}
\references{ 
Stanley, C.R., 2003. Statistical evaluation of anomaly recognition performance.  Geochemistry: Exploration, Environment, Analaysis, 3(1):3-12.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.runs}} }
\examples{
## From Stanley (2003) Tables 1 and 2

gx.hypergeom(31, 10, 5, 3)
gx.hypergeom(31, 10, 3, 2)
gx.hypergeom(31, 10, 4, 3)

gx.hypergeom(31, 10, 4, 4)
gx.hypergeom(31, 10, 6, 5)
gx.hypergeom(31, 10, 3, 3)
}
\keyword{ htest }

