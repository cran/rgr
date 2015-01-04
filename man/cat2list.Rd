\name{cat2list}
\alias{cat2list}
\title{ Divides Data into Subsets by Factor }
\description{
Converts data into a list form where data are grouped together by factor.  Achieves the same objective as the base function \code{\link{split}}.
}
\usage{
cat2list(x, a)
}
\arguments{
  \item{x}{ name of the data variable to be processed. }
  \item{a}{ name of the factor variable by which the data are to be split. }
}
\value{
  \item{data}{ a list containing factors as columns and the values for those factors as rows.  The order of the resulting groups, subsets, is the order in which the factor variable names were encountered in parameter \code{a} passed to the function. }
}
\note{
This function is called by functions \code{\link{tbplots}} and \code{\link{bwplots}} to prepare Tukey boxplots and box-and-whisker plots, respectively. It is an integral part of the script shared by Doug Nychka on S-News, April 28, 1992.  As such it may pre-date the time that \code{\link{split}} was added to the S-Plus library.

If \code{by} is undefined in the calling functions, \code{\link{tbplots}} and \code{\link{bwplots}}, the same result may be achieved by using the \code{split(x, a)} construct instead of stating \code{x} as the variable to be displayed as boxplots.  In which case the data are grouped, subsetted, in alphabetical order of factor variable names.
}
\author{ Douglas W. Nychka }
\keyword{ misc }
 
