\name{fences}
\alias{fences}
\title{ Generate and Display Fence Values }
\description{
Function to generate fence values to support the selection of the upper and lower bounds of background variability, i.e. threshold(s) or action levels, when an obvious graphical solution is not visually recognizable.
}
\usage{
fences(xx, display = TRUE)
}
\arguments{
  \item{xx}{ name of the variable to be processed. }
  \item{display}{ the default is to display the tabular output on the current device, i.e. \code{display = TRUE}. However, when the function is used in conjunction with \code{\link{fences.summary}} then \code{display = FALSE} in order to suppress output to the current device as it will be saved to a text file for subsequent use/editing and reference. }
}
\details{
The fence values are computed by several procedures both with and without a logarithic data transformation, together with the 98th percentile of the data for display.  These computations are based on results returned from function \code{\link{gx.stats}}. Fences are computed following Tukey's boxplot procedure, as median ± 2 * MAD (Median Absolute Deviation), and mean ± 2 * SD (Standard Deviation), see Reimann et al. (2005). It is essential that these estimates are viewed in the context of the graphical distributional displays, e.g., \code{\link{shape}} and its graphical components, \code{\link{gx.hist}}, \code{\link{gx.ecdf}}, \code{\link{cnpplt}} and \code{\link{bxplot}}, and if spatial coordinates for the sample sites are available \code{\link{edamap7}}, \code{\link{edamap8}} and \code{\link{caplot}}. The final selection of a range for background or the selection of a threshold level needs to take the statistical and spatial distributions of the data into account. It is also necessary to be aware that it might be appropriate to have more than one background range/threshold in an area (Reimann and Garrett, 2005). The presence of relevant information in the data frame may permit the data to be subset on the basis of criteria using the \code{\link{tbplot}} and \code{\link{bwplot}} functions. If these indicate that the medians and middle 50\%s of the data are visibly different, multiple background ranges may be advisable.
}
\references{
Reimann, C. and Garrett, R.G., 2005. Geochemical background - Concept and reality. Science of the Total Environment, 350(1-3):12-27. 

Reimann, C., Filzmoser, P. and Garrett, R.G., 2005. Background and threshold: critical comparison of methods of determination.  Science of the Total Environment, 346(1-3):1-16. 
}
\author{ Robert G. Garrett }
\note{  
Any less than detection limit values represented by negative values or zeros or other numeric codes representing blanks in the data vector must be removed prior to executing this function, see \code{\link{ltdl.fix.df}}.

Any \code{NA}s in the data vector are removed prior to computing the fences.
}
\seealso{ \code{\link{gx.stats}}, \code{\link{fences.summary}}, \code{\link{ltdl.fix.df}}, \code{\link{remove.na}} }
\examples{
## Make test data available
data(kola.o)
attach(kola.o)

## Display the fences computed for Cu
fences(Cu)

## Detach test data
detach(kola.o)     
}
\keyword{ univar }

