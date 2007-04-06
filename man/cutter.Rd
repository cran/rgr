\name{cutter}
\alias{cutter}
\title{ Function to Identify in Which Interval a Value Falls }
\description{
Function to identify in which interval of a set of cut points, cuts, a value x falls within or beyond. The number of intervals is equal to the number of cut points plus 1. Values of x have to exceed the value of the cut point to be allocated to the higher interval.
}
\usage{
cutter(x, cuts)
}
\arguments{
  \item{x}{ name of the vector to be processed. }
  \item{cuts}{ the vector of cut points. }
}
\value{
  \item{xi}{ a vector of the same length as x containing an integer between 1 and the number of cut points plus 1 indicating in which interval each value of x fell.  Values \code{<}cut[1] have \code{xi} set to 1, and values \code{>}cut[highest] have \code{xi} set to the number of cut points plus 1. }
}
\author{ Robert G. Garrett }
\examples{
## Make test data available
data(kola.c)
attach(kola.c)

## Cut the data into quartiles
xi <- cutter(Cu, quantile(Cu, probs = c(0.25, 0.5, 0.75)))

## Detach test data
detach(kola.c)
}
\keyword{ misc }
 

