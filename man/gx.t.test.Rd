\name{gx.t.test}
\alias{gx.t.test}
\alias{t.test}
\alias{t-test}
\title{ t-test from Means, Standard Deviations and Sample Sizes }
\description{
Function to test hypothesis of equality of two means given the mean, standard deviation and size (N) of each population. 
}
\usage{
gx.t.test(xbar1, s1, n1, xbar2, s2, n2)
}
\arguments{
  \item{xbar1}{ mean of population 1. }
  \item{s1}{ standard deviation of population 1. }
  \item{n1}{ size of population 1. }
  \item{xbar2}{ mean of population 2. }
  \item{s2}{ stabdard deviation of population 2. }
  \item{n2}{ size of population 2. }
}
\details{ 
Function written to support QA/QC studies where data for sets of Control Reference Material analyses are to be compared, e.g., between two projects, or when comparing published statistics with those from a project.

The hypothesis, H0: mean1 = mean2, is tested at the 95\% confidence level and the user informed whether the hypothesis is accepted or rejected.  Additionally the difference, t-value, degrees of freedom, and p-value are displayed.
}
\author{ Robert G. Garrett }
\examples{
gx.t.test(22.3, 4.6, 25, 24.2, 3.8, 22)
gx.t.test(22.3, 4.6, 25, 26.2, 3.8, 22)
}
\keyword{ htest }

