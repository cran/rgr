\name{gx.runs}
\alias{gx.runs}
\title{ The Wald-Wolfowitz, \sQuote{Runs}, Test }
\description{  The \sQuote{runs} test is used to infer whether two states, e.g., > and < some threshold are mutually independent along a traverse.  In applied geochemical terms, it tests for pattern coherence.  If the pattern of runs is not coherent at the scale of the sampling it will be difficult to identify any spatially consistent dispersion processes.
}
\usage{
gx.runs(n1, n2, u)
}
\arguments{
  \item{n1}{ the number of < threshold sites along a traverse. }
  \item{n2}{ the number of > threshold sites along a traverse. }
  \item{u}{ the number of runs of > and < threshold sites along the traverse. }
}
\note{
Given a priori information on the location of a mineral occurrence, the \code{\link{gx.hypergeom}} function provides a far more insightful test.  The \sQuote{runs} test is better suited for evaluating patterns due to lithological or environmental changes along a traverse when some \sQuote{threshold} can be selected that differentiates between two patterns
}
\references{
Stanley, C.R., 2003. Statistical evaluation of anomaly recognition performance.  Geochemistry: Exploration, Environment, Analaysis, 3(1):3-12.
}
\author{ Robert G. Garrett }
\seealso{ \code{\link{gx.hypergeom}} }
\examples{
## From Stanley (2003) Table 2

gx.runs(27, 4, 7)
gx.runs(25, 6, 7)
gx.runs(28, 3, 5)
}
\keyword{ htest }
\keyword{ nonparametric }
