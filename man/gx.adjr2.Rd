\name{gx.adjr2}
\alias{gx.adjr2}
\title{ Function to compute Adjusted r-squared values }
\description{
Function to compute the Adjusted R-square value from the Multiple R-squared value displayed in the \code{\link{summary}} of a \code{\link{lm}} object.  See Note below.
}
\usage{
gx.adjr2(mr2, n, p)
}
\arguments{
  \item{mr2}{ the Multiple R-squared value. }
  \item{n}{ the number of cases in the regression model. }
  \item{p}{ the number of independent (explanatory or predictor) variables in the model. }
}
\note{
The Adjusted R-squared value is a long established criterion.  It may be calculated casually by this function, or may be extracted from a \code{\link{lm}} object, using \code{summary(lm.object)[[9]]}.  However, users are urged to investigate Akaike's Information Criterion, \code{\link{AIC}}, as a procedure for comparing the fits of alternate models, and the use of the \code{\link{step}} function for automated model selection.
}

\author{ Robert G. Garrett }
\seealso{ \code{\link{summary}}, \code{\link{AIC}}, \code{\link{step}} }
\examples{
gx.adjr2(0.7394, 111, 11)
gx.adjr2(0.713, 111, 6)
}
\keyword{ models }

